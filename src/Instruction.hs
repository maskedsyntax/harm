module Instruction where

import ARM7 (Register)
import Data.Word (Word32)

-- | Condition Codes
data Condition
    = EQ' | NE | CS | CC | MI | PL | VS | VC | HI | LS | GE | LT' | GT' | LE | AL
    deriving (Show, Eq, Enum, Bounded)

-- | Operands for instructions
data Operand
    = Imm Word32      -- ^ Immediate value
    | Reg Register    -- ^ Register value
    deriving (Show, Eq)

-- | ARM7 Instruction Set (Subset)
data Instruction
    = MOV Condition Bool Register Operand -- ^ MOV {cond} {S} dst, src
    | MVN Condition Bool Register Operand -- ^ MVN {cond} {S} dst, src (NOT)
    | ADD Condition Bool Register Register Operand -- ^ ADD {cond} {S} dst, src1, src2
    | SUB Condition Bool Register Register Operand
    | RSB Condition Bool Register Register Operand
    | AND Condition Bool Register Register Operand
    | ORR Condition Bool Register Register Operand
    | EOR Condition Bool Register Register Operand
    | BIC Condition Bool Register Register Operand
    | MUL Condition Bool Register Register Register
    | CMP Condition Register Operand
    | LDR Condition Register Register -- ^ LDR Rd, [Rn]
    | STR Condition Register Register -- ^ STR Rd, [Rn]
    | B   Condition Target -- ^ Branch to target
    deriving (Show, Eq)

data Target = ImmAddr Word32 | TLabel String deriving (Show, Eq)
