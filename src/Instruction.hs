module Instruction where

import ARM7 (Register)
import Data.Word (Word32)

-- | Condition Codes
data Condition
    = EQ' | NE | CS | CC | MI | PL | VS | VC | HI | LS | GE | LT | GT | LE | AL
    deriving (Show, Eq, Enum, Bounded)

-- | Operands for instructions
data Operand
    = Imm Word32      -- ^ Immediate value
    | Reg Register    -- ^ Register value
    deriving (Show, Eq)

-- | ARM7 Instruction Set (Subset)
data Instruction
    = MOV Condition Register Operand
    | ADD Condition Register Register Operand
    | SUB Condition Register Register Operand
    | RSB Condition Register Register Operand
    | AND Condition Register Register Operand
    | ORR Condition Register Register Operand
    | EOR Condition Register Register Operand
    | BIC Condition Register Register Operand
    | MUL Condition Register Register Register
    | CMP Condition Register Operand
    | B   Condition Word32 -- ^ Branch to address/offset
    deriving (Show, Eq)
