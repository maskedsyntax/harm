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
    let parsed = map parseLineContent lines'
    case sequence parsed of
        Left err -> putStrLn $ "Error parsing file:\n" ++ err
        Right contents -> do
            let (instMap, labelMap) = assemble contents
            let resolvedInstMap = Map.map (resolveLabels labelMap) instMap
            let finalState = runProgram resolvedInstMap initCPU
            putStrLn "Final Register State:"
            printRegisters finalState

assemble :: [Maybe LineContent] -> (Map.Map Word32 Instruction, Map.Map String Word32)
assemble contents = go 0 Map.empty Map.empty contents
  where
    go _ insts labels [] = (insts, labels)
    go addr insts labels (Nothing : rest) = go addr insts labels rest
    go addr insts labels (Just (LLabel name) : rest) = go addr insts (Map.insert name addr labels) rest
    go addr insts labels (Just (LInstruction inst) : rest) = 
        go (addr + 4) (Map.insert addr inst insts) labels rest

resolveLabels :: Map.Map String Word32 -> Instruction -> Instruction
resolveLabels labels (B cond (TLabel name)) = 
    case Map.lookup name labels of
        Just addr -> B cond (ImmAddr addr)
        Nothing -> error $ "Undefined label: " ++ name
resolveLabels _ inst = inst

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
        _      -> case parseLineContent input of
            Left err -> do
                putStrLn $ "Error: " ++ err
                repl state
            Right Nothing -> repl state
            Right (Just (LLabel _)) -> repl state -- Ignore labels in REPL for now
            Right (Just (LInstruction instr)) -> do
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
