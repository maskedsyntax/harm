module Execute where

import ARM7
import Instruction
import Data.Word (Word32)
import Data.Bits ((.&.), (.|.), xor, complement)

-- | Check if the instruction should execute based on current flags
checkCondition :: Flags -> Condition -> Bool
checkCondition flags cond = case cond of
    AL -> True
    EQ' -> zFlag flags
    NE -> not (zFlag flags)
    CS -> cFlag flags
    CC -> not (cFlag flags)
    MI -> nFlag flags
    PL -> not (nFlag flags)
    VS -> vFlag flags
    VC -> not (vFlag flags)
    HI -> cFlag flags && not (zFlag flags)
    LS -> not (cFlag flags) || zFlag flags
    GE -> nFlag flags == vFlag flags
    LT' -> nFlag flags /= vFlag flags
    GT' -> not (zFlag flags) && (nFlag flags == vFlag flags)
    LE -> zFlag flags || (nFlag flags /= vFlag flags)

-- | Evaluate an operand to get its 32-bit value
evalOperand :: CPUState -> Operand -> Word32
evalOperand _ (Imm val)     = val
evalOperand state (Reg r)   = getReg state r

-- | Execute a single instruction
execute :: Instruction -> CPUState -> CPUState
execute inst state = case inst of
    MOV cond s dst src -> 
        if checkCondition (cpsr state) cond
        then let val = evalOperand state src
                 state' = setReg state dst val
             in if s then updateFlags state' val else state'
        else state
    MVN cond s dst src -> 
        if checkCondition (cpsr state) cond
        then let val = complement (evalOperand state src)
                 state' = setReg state dst val
             in if s then updateFlags state' val else state'
        else state
    ADD cond s dst src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = evalOperand state src2
                 res = v1 + v2
                 state' = setReg state dst res
             in if s then updateFlags state' res else state'
        else state
    SUB cond s dst src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = evalOperand state src2
                 res = v1 - v2
                 state' = setReg state dst res
             in if s then updateFlags state' res else state'
        else state
    AND cond s dst src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = evalOperand state src2
                 res = v1 .&. v2
                 state' = setReg state dst res
             in if s then updateFlags state' res else state'
        else state
    ORR cond s dst src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = evalOperand state src2
                 res = v1 .|. v2
                 state' = setReg state dst res
             in if s then updateFlags state' res else state'
        else state
    EOR cond s dst src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = evalOperand state src2
                 res = v1 `xor` v2
                 state' = setReg state dst res
             in if s then updateFlags state' res else state'
        else state
    BIC cond s dst src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = evalOperand state src2
                 res = v1 .&. complement v2
                 state' = setReg state dst res
             in if s then updateFlags state' res else state'
        else state
    RSB cond s dst src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = evalOperand state src2
                 res = v2 - v1
                 state' = setReg state dst res
             in if s then updateFlags state' res else state'
        else state
    MUL cond s dst src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = getReg state src2
                 res = v1 * v2
                 state' = setReg state dst res
             in if s then updateFlags state' res else state'
        else state
    CMP cond src1 src2 ->
        if checkCondition (cpsr state) cond
        then let v1 = getReg state src1
                 v2 = evalOperand state src2
                 res = v1 - v2
             in updateFlags state res
        else state
    B cond target ->
        if checkCondition (cpsr state) cond
        then setReg state PC target
        else state
    LDR cond dst baseReg ->
        if checkCondition (cpsr state) cond
        then let addr = getReg state baseReg
                 val = readMem state addr
             in setReg state dst val
        else state
    STR cond src baseReg ->
        if checkCondition (cpsr state) cond
        then let addr = getReg state baseReg
                 val = getReg state src
             in writeMem state addr val
        else state
    _ -> state

-- | Simple flag update for N and Z flags (C and V are more complex)
updateFlags :: CPUState -> Word32 -> CPUState
updateFlags state res = 
    let oldFlags = cpsr state
        newFlags = oldFlags 
            { nFlag = res >= 0x80000000
            , zFlag = res == 0
            }
    in state { cpsr = newFlags }
