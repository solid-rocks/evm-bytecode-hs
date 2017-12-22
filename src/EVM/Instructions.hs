{-# LANGUAGE TemplateHaskell #-}
module EVM.Instructions where

import           Prelude hiding (Ordering(..))
import           Data.Int (Int64)
import           Data.Word
import           Data.List (foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Text.Printf

import EVM.InstructionsTH


$(generateInstructionsData "Instruction")

instructionMap :: Map Word8 (ByteString -> Int64 -> Instruction)
instructionMap = Map.fromList $(generateInstructionCons)


-- Using ByteString and index (instead of list of bytes)
-- allows us to follow JUMP instructions in sublinear time.
readOne :: ByteString -> Int64 -> Instruction
readOne bytecode index =
  case LB.index bytecode index of
    byte -> case Map.lookup byte instructionMap of
      Nothing -> INVALID $ printf "Invalid bytecode 0x%02x" byte
      Just op -> op bytecode (index + 1)


size :: Integral a => Instruction -> a
size = \case
  PUSH n _ -> 1 + fromIntegral n
  _ -> 1


readProgram :: ByteString -> Int64 -> [Instruction]
readProgram str = go
  where
    go offset
      | offset < 0 || offset >= LB.length str = []
      | otherwise = case readOne str offset of
        op@(INVALID err) -> [op]
        op -> op : go (offset + size op)


-- "Offset is out of range: " ++ show offset
-- "Not enough bytes: 0x" ++ showHex bytes

