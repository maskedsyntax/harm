import ARM7
import Parser
import Execute
import Instruction
import qualified Data.Map as Map
import Data.Word (Word32)
import System.Exit (exitFailure, exitSuccess)

-- | Simple test runner
test :: String -> CPUState -> (CPUState -> Bool) -> IO Bool
test name state check = do
    putStr $ "Testing " ++ name ++ "... "
    if check state
        then putStrLn "PASSED" >> return True
        else putStrLn "FAILED" >> return False

main :: IO ()
main = do
    results <- sequence
        [ test "Initial CPU State" initCPU (\s -> getReg s R0 == 0)
        , test "MOV Instruction" (execute (MOV AL False R1 (Imm 42)) initCPU) (\s -> getReg s R1 == 42)
        , test "ADD Instruction" 
            (let s1 = setReg initCPU R1 10
                 s2 = setReg s1 R2 20
             in execute (ADD AL False R3 R1 (RegShift R2 Nothing)) s2)
            (\s -> getReg s R3 == 30)
        , test "Flag Z Update"
            (execute (MOV AL True R1 (Imm 0)) initCPU)
            (\s -> zFlag (cpsr s))
        , test "Flag N Update"
            (execute (MOV AL True R1 (Imm 0x80000000)) initCPU)
            (\s -> nFlag (cpsr s))
        , test "Memory LDR/STR"
            (let s1 = setReg initCPU R1 0x100
                 s2 = setReg s1 R2 0xABC
                 s3 = execute (STR AL R2 R1) s2
             in execute (LDR AL R3 R1) s3)
            (\s -> getReg s R3 == 0xABC)
        ]
    if all id results
        then do
            putStrLn "\nAll tests passed!"
            exitSuccess
        else do
            putStrLn "\nSome tests failed."
            exitFailure
