module ARM7 where

import Data.Word (Word32)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Virtual I/O addresses
uartAddr :: Word32
uartAddr = 0x10000000

-- | ARM7 Registers
data Register
    = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    | SP -- Alias for R13
    | LR -- Alias for R14
    | PC -- Alias for R15
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | CPSR (Current Program Status Register) Flags
data Flags = Flags
    { nFlag :: Bool -- Negative
    , zFlag :: Bool -- Zero
    , cFlag :: Bool -- Carry
    , vFlag :: Bool -- Overflow
    } deriving (Show, Eq)

-- | Initial empty flags
defaultFlags :: Flags
defaultFlags = Flags False False False False

-- | CPU State
data CPUState = CPUState
    { registers   :: Map.Map Register Word32
    , cpsr        :: Flags
    , memory      :: Map.Map Word32 Word32 -- Simplified memory model (Address -> Word)
    , breakpoints :: Set.Set Word32
    } deriving (Show, Eq)

-- | Initialize CPU with all registers at 0
initCPU :: CPUState
initCPU = CPUState
    { registers = Map.fromList [(r, 0) | r <- [R0 .. R15]]
    , cpsr      = defaultFlags
    , memory    = Map.empty
    , breakpoints = Set.empty
    }

-- | Helper to get register value
getReg :: CPUState -> Register -> Word32
getReg state reg = Map.findWithDefault 0 (normalizeReg reg) (registers state)
  where
    normalizeReg SP = R13
    normalizeReg LR = R14
    normalizeReg PC = R15
    normalizeReg r  = r

-- | Helper to set register value
setReg :: CPUState -> Register -> Word32 -> CPUState
setReg state reg val = state { registers = Map.insert (normalizeReg reg) val (registers state) }
  where
    normalizeReg SP = R13
    normalizeReg LR = R14
    normalizeReg PC = R15
    normalizeReg r  = r

-- | Helper to read memory
readMem :: CPUState -> Word32 -> Word32
readMem state addr = Map.findWithDefault 0 addr (memory state)

-- | Helper to write memory
writeMem :: CPUState -> Word32 -> Word32 -> CPUState
writeMem state addr val = state { memory = Map.insert addr val (memory state) }
