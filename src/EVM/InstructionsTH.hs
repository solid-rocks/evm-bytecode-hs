{-# LANGUAGE TemplateHaskell #-}

module EVM.InstructionsTH where

import           Data.Int (Int64)
import           Data.Word
import           Data.LargeWord (Word256, LargeKey)
import           Data.Bits
import           Data.List (foldl')
import qualified Data.ByteString.Lazy as LBS
import           Language.Haskell.TH


data InstructionDesc = InstructionDesc
  { code       :: Word8
  , name       :: String
  , variants   :: [Integer]
  , arguments  :: [Name]
  , deprecated :: Bool
  , metropolis :: Bool
  }


descriptions :: [InstructionDesc]
descriptions = let i c n = InstructionDesc c n [] [] False False in
  [ i 0x00 "STOP"
  , i 0x01 "ADD"
  , i 0x02 "MUL"
  , i 0x03 "SUB"
  , i 0x04 "DIV"
  , i 0x05 "SDIV"
  , i 0x06 "MOD"
  , i 0x07 "SMOD"
  , i 0x08 "ADDMOD"
  , i 0x09 "MULMOD"
  , i 0x0a "EXP"
  , i 0x0b "SIGNEXTEND"
  , i 0x10 "LT"
  , i 0x11 "GT"
  , i 0x12 "SLT"
  , i 0x13 "SGT"
  , i 0x14 "EQ"
  , i 0x15 "ISZERO"
  , i 0x16 "AND"
  , i 0x17 "OR"
  , i 0x18 "XOR"
  , i 0x19 "NOT"
  , i 0x1a "BYTE"
  , i 0x20 "SHA3"
  , i 0x30 "ADDRESS"
  , i 0x31 "BALANCE"
  , i 0x32 "ORIGIN"
  , i 0x33 "CALLER"
  , i 0x34 "CALLVALUE"
  , i 0x35 "CALLDATALOAD"
  , i 0x36 "CALLDATASIZE"
  , i 0x37 "CALLDATACOPY"
  , i 0x38 "CODESIZE"
  , i 0x39 "CODECOPY"
  , i 0x3a "GASPRICE"
  , i 0x3b "EXTCODESIZE"
  , i 0x3c "EXTCODECOPY"
  ,(i 0x3d "RETURNDATASIZE") {metropolis = True}
  ,(i 0x3e "RETURNDATACOPY") {metropolis = True}
  , i 0x40 "BLOCKHASH"
  , i 0x41 "COINBASE"
  , i 0x42 "TIMESTAMP"
  , i 0x43 "NUMBER"
  , i 0x44 "DIFFICULTY"
  , i 0x45 "GASLIMIT"
  , i 0x50 "POP"
  , i 0x51 "MLOAD"
  , i 0x52 "MSTORE"
  , i 0x53 "MSTORE8"
  , i 0x54 "SLOAD"
  , i 0x55 "SSTORE"
  , i 0x56 "JUMP"
  , i 0x57 "JUMPI"
  , i 0x58 "PC"
  , i 0x59 "MSIZE"
  , i 0x5a "GAS"
  , i 0x5b "JUMPDEST"
  ,(i 0x60 "PUSH")           {variants = [1..16], arguments = [''Word256]}
  ,(i 0x80 "DUP")            {variants = [1..16]}
  ,(i 0x90 "SWAP")           {variants = [1..16]}
  ,(i 0xa0 "LOG")            {variants = [0..4]}
  ,(i 0xe1 "SLOADBYTES")     {deprecated = True}
  ,(i 0xe2 "SSTOREBYTES")    {deprecated = True}
  ,(i 0xe3 "SSIZE")          {deprecated = True}
  , i 0xf0 "CREATE"
  , i 0xf1 "CALL"
  , i 0xf2 "CALLCODE"
  , i 0xf3 "RETURN"
  , i 0xf4 "DELEGATECALL"
  , i 0xf5 "CALLBLACKBOX"
  , i 0xfa "STATICCALL"
  , i 0xfe "ASSERT_FAIL"
  ,(i 0xfd "REVERT")         {metropolis = True} -- EIP-150
  , i 0xff "SUICIDE"
  ]


generateInstructionsData :: String -> Q [Dec]
generateInstructionsData nm
  = fmap (:[])
  $ dataD
    (cxt [])    -- context
    (mkName nm) -- name
    []          -- type variables
    Nothing     -- kind
    ( normalC (mkName "INVALID") [strictArg ''Word8]
    : map (\x -> normalC (mkName $ name x) (args x))
      descriptions
    )
    (cxt [conT ''Show, conT ''Eq])
  where
    strictArg = bangType (bang sourceUnpack sourceStrict) . conT
    args i = case variants i of
      [] -> map strictArg $ arguments i
      _  -> map strictArg $ ''Int : arguments i


data ParserError
  = PositionIsOutOfRange
  deriving Show


readWordFromLBS :: LBS.ByteString -> Int64 -> Int -> Word256
readWordFromLBS bs ix sz
  = foldl' (\res b -> unsafeShiftL res 8 .|. fromIntegral b) 0
  $ map (LBS.index bs) [ix .. ix + fromIntegral sz - 1]


generateInstructionParser :: Q Exp
generateInstructionParser = do
  bs <- newName "bytecode"
  ix <- newName "offset"

  -- make constructor from instruction desc
  let ctr = ConE . mkName . name

  -- unfold instruction descriptions like PUSH*, DUP*
  let unfoldVariants i = case variants i of
        [] -> [(fromIntegral $ code i, ctr i, 0)]
        vs ->
          [(ix + fromIntegral (code i)
           ,AppE (ctr i) (LitE $ IntegerL  v)
           ,v)
          | (ix, v) <- zip [0..] vs
          ]

  let matchCode c exp = Match (LitP $ IntegerL c) (NormalB exp) []
  readWord <- runQ [e| readWordFromLBS $(varE bs) ($(varE ix) + 1) |]

  let caseMatches =
        [ matchCode c $ TupE [exp', LitE $ IntegerL $ argSz + 1]
        | i <- descriptions
        , (c, exp, argSz) <- unfoldVariants i
        -- add argument to PUSH
        , let exp' = if null $ arguments i
              then exp
              else AppE exp (AppE readWord (LitE $ IntegerL $ argSz))
        ]

  caseExp <- runQ [e| LBS.index $(varE bs) $(varE ix)|]
  return
    $ LamE [VarP bs, VarP ix] $ CaseE caseExp caseMatches