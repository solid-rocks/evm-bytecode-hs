{-# LANGUAGE TemplateHaskell #-}
module EVM.Instructions where

import           Prelude hiding (Ordering(..))
import           Data.Int (Int64)
import           Data.Word
import           Data.List (foldl')
import           Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy

import EVM.InstructionsTH





codeMap :: IntMap InstructionDesc
codeMap = foldl' (\m x -> Map.insert (fromIntegral $ code x) x m) Map.empty descriptions

$(generateInstructionsData "Instruction")
-- INVALID Word8

readInstruction :: ByteString -> Int64 -> (Instruction, Int)
readInstruction = $(generateInstructionParser)

-- "Offset is out of range: " ++ show offset
-- "Not enough bytes: 0x" ++ showHex bytes

