module EVM.Instructions where

import           Prelude hiding (Ordering(..))
import           Data.List (foldl')
import           Data.Int (Int64)
import           Data.Word (Word8)
import           Data.LargeWord (Word256)
import           Data.Bits ((.|.), unsafeShiftL)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Text.Printf


data Instr
  = STOP
  | ADD | MUL | SUB | DIV | SDIV
  | MOD | SMOD | ADDMOD | MULMOD | EXP
  | SIGNEXTEND
  | LT | GT | SLT | SGT | EQ | ISZERO
  | AND | OR | XOR | NOT
  | BYTE
  | SHA3
  | ADDRESS | BALANCE | ORIGIN | CALLER
  | CALLVALUE | CALLDATALOAD | CALLDATASIZE | CALLDATACOPY
  | CODESIZE | CODECOPY
  | GASPRICE
  | EXTCODESIZE | EXTCODECOPY
  | RETURNDATASIZE | RETURNDATACOPY
  | BLOCKHASH | COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT
  | POP
  | MLOAD | MSTORE | MSTORE8
  | SLOAD | SSTORE
  | JUMP | JUMPI
  | PC | MSIZE | GAS
  | JUMPDEST
  | PUSH | DUP | SWAP
  | LOG
  | SLOADBYTES | SSTOREBYTES | SSIZE
  | CREATE
  | CALL | CALLCODE
  | RETURN
  | DELEGATECALL | CALLBLACKBOX | STATICCALL
  | ASSERT_FAIL | REVERT
  | SUICIDE
  deriving (Show, Eq)


data Instruction
  = Instruction
    { instr     :: Instr
    , variant   :: Maybe Int
    , argument  :: Maybe Word256
    }
  | Invalid
    { byteCode :: Word8
    , reason   :: String
    }

instance Show Instruction where
  show = \case
    Instruction ins var arg
      -> show ins
        ++ maybe "" show var
        ++ maybe "" (printf " 0x%02x" . toInteger) arg
    Invalid byte desc -> printf "INVALID 0x%02x -- %s" byte desc


type Offset = Int64

instructionMap :: Map Word8 (LBS.ByteString -> Offset -> Instruction)
instructionMap = Map.fromList
  $ [(0x00, mk STOP)
    ,(0x01, mk ADD)
    ,(0x02, mk MUL)
    ,(0x03, mk SUB)
    ,(0x04, mk DIV)
    ,(0x05, mk SDIV)
    ,(0x06, mk MOD)
    ,(0x07, mk SMOD)
    ,(0x08, mk ADDMOD)
    ,(0x09, mk MULMOD)
    ,(0x0a, mk EXP)
    ,(0x0b, mk SIGNEXTEND)
    ,(0x10, mk LT)
    ,(0x11, mk GT)
    ,(0x12, mk SLT)
    ,(0x13, mk SGT)
    ,(0x14, mk EQ)
    ,(0x15, mk ISZERO)
    ,(0x16, mk AND)
    ,(0x17, mk OR)
    ,(0x18, mk XOR)
    ,(0x19, mk NOT)
    ,(0x1a, mk BYTE)
    ,(0x20, mk SHA3)
    ,(0x30, mk ADDRESS)
    ,(0x31, mk BALANCE)
    ,(0x32, mk ORIGIN)
    ,(0x33, mk CALLER)
    ,(0x34, mk CALLVALUE)
    ,(0x35, mk CALLDATALOAD)
    ,(0x36, mk CALLDATASIZE)
    ,(0x37, mk CALLDATACOPY)
    ,(0x38, mk CODESIZE)
    ,(0x39, mk CODECOPY)
    ,(0x3a, mk GASPRICE)
    ,(0x3b, mk EXTCODESIZE)
    ,(0x3c, mk EXTCODECOPY)
    ,(0x3d, mk RETURNDATASIZE)
    ,(0x3e, mk RETURNDATACOPY)
    ,(0x40, mk BLOCKHASH)
    ,(0x41, mk COINBASE)
    ,(0x42, mk TIMESTAMP)
    ,(0x43, mk NUMBER)
    ,(0x44, mk DIFFICULTY)
    ,(0x45, mk GASLIMIT)
    ,(0x50, mk POP)
    ,(0x51, mk MLOAD)
    ,(0x52, mk MSTORE)
    ,(0x53, mk MSTORE8)
    ,(0x54, mk SLOAD)
    ,(0x55, mk SSTORE)
    ,(0x56, mk JUMP)
    ,(0x57, mk JUMPI)
    ,(0x58, mk PC)
    ,(0x59, mk MSIZE)
    ,(0x5a, mk GAS)
    ,(0x5b, mk JUMPDEST)
    ]
  ++[(0x60 + i - 1, mkPush i) | i <- [1..16]]
  ++[(0x80 + i - 1, mkVariant i DUP) | i <- [1..16]]
  ++[(0x90 + i - 1, mkVariant i SWAP) | i <- [1..16]]
  ++[(0xa0 + i, mkVariant i LOG) | i <- [0..4]]
  ++[(0xe1, mk SLOADBYTES)
    ,(0xe2, mk SSTOREBYTES)
    ,(0xe3, mk SSIZE)
    ,(0xf0, mk CREATE)
    ,(0xf1, mk CALL)
    ,(0xf2, mk CALLCODE)
    ,(0xf3, mk RETURN)
    ,(0xf4, mk DELEGATECALL)
    ,(0xf5, mk CALLBLACKBOX)
    ,(0xfa, mk STATICCALL)
    ,(0xfe, mk ASSERT_FAIL)
    ,(0xfd, mk REVERT)
    ,(0xff, mk SUICIDE)
    ]
    where
      mk i _ _ = Instruction i Nothing Nothing
      mkVariant v i _ _ = Instruction i (Just $ fromIntegral v) Nothing
      mkPush sz bytecode offset
        | offset < 0 || offset + 1 + (fromIntegral sz) > LBS.length bytecode
          = Invalid instrByte (printf  "Not enough bytes for PUSH%i" sz)
        | otherwise
          = Instruction PUSH (Just $ fromIntegral sz) (Just arg)
          where
            instrByte = LBS.index bytecode offset
            arg = foldl' (\res b -> unsafeShiftL res 8 .|. fromIntegral b) 0
              $ map (LBS.index bytecode) [offset + 1 .. offset + fromIntegral sz]


size :: Integral a => Instruction -> a
size = \case
  Instruction PUSH (Just sz) _ -> 1 + fromIntegral sz
  _ -> 1


-- Using ByteString and offset (instead of list of bytes)
-- allows us to follow JUMP instructions in sublinear time.
readProgram :: ByteString -> Offset -> [(Offset, Instruction)]
readProgram bytecode = go
  where
    go offset
      | offset < 0 || offset >= LBS.length bytecode = []
      | otherwise = (offset, inst) : go (offset + size inst)
      where
        byte = LBS.index bytecode offset
        inst = case Map.lookup byte instructionMap of
          Just fn -> fn bytecode offset
          Nothing -> Invalid byte "Unknown bytecode"
