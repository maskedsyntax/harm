module Main where

import ARM7
import Parser
import Execute
import Instruction
import Data.Word (Word32)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map
import Text.Printf (printf)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> runFile filename
        [] -> do
            putStrLn "HARM (Haskell ARM7 Emulator) - Type 'exit' to quit."
            repl initCPU
        _ -> putStrLn "Usage: harm [filename]"

runFile :: FilePath -> IO ()
runFile path = do
    content <- readFile path
    let lines' = lines content
    let parsed = map parseLine lines'
    case sequence parsed of
        Left err -> putStrLn $ "Error parsing file:\n" ++ err
        Right maybeInsts -> do
            let insts = [i | Just i <- maybeInsts]
            let instMap = Map.fromList $ zip [0, 4 ..] insts
            let finalState = runProgram instMap initCPU
            putStrLn "Final Register State:"
            printRegisters finalState

runProgram :: Map.Map Word32 Instruction -> CPUState -> CPUState
runProgram insts state =
    let pcVal = getReg state PC
    in case Map.lookup pcVal insts of
        Nothing -> state -- Halt if PC points to no instruction
        Just inst ->
            let state' = setReg state PC (pcVal + 4) -- Auto-increment PC
                state'' = execute inst state'
            in runProgram insts state''

repl :: CPUState -> IO ()
repl state = do
    putStr "harm> "
    hFlush stdout
    input <- getLine
    let cmd = head (words input ++ [""])
    case cmd of
        "exit" -> putStrLn "Goodbye!"
        "dump" -> do
            printMemory state
            repl state
        ""     -> repl state
        _      -> case parseLine input of
            Left err -> do
                putStrLn $ "Error: " ++ err
                repl state
            Right Nothing -> repl state
            Right (Just instr) -> do
                let state' = execute instr state
                printRegisters state'
                repl state'

printMemory :: CPUState -> IO ()
printMemory state = do
    let mem = Map.toList (memory state)
    if null mem
        then putStrLn "Memory is empty."
        else mapM_ (\(addr, val) -> printf "0x%08X: 0x%08X\n" addr val) mem

printRegisters :: CPUState -> IO ()
printRegisters state = do
    let regs = Map.toList (registers state)
    mapM_ (\(r, v) -> printf "%-3s: 0x%08X  " (show r) v) (take 8 regs)
    putChar '\n'
    mapM_ (\(r, v) -> printf "%-3s: 0x%08X  " (show r) v) (drop 8 regs)
    putChar '\n'
    print (cpsr state)
