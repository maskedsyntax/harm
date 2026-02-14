module Main where

import ARM7
import Parser
import Execute
import Instruction
import Data.Word (Word32)
import System.IO (hFlush, stdout)
import Data.List (isPrefixOf)
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
resolveLabels labels (BL cond (TLabel name)) = 
    case Map.lookup name labels of
        Just addr -> BL cond (ImmAddr addr)
        Nothing -> error $ "Undefined label: " ++ name
resolveLabels labels (ADR cond reg name) =
    case Map.lookup name labels of
        Just addr -> MOV cond False reg (Imm addr)
        Nothing -> error $ "Undefined label: " ++ name
resolveLabels _ (LDRPseudo cond reg val) = MOV cond False reg (Imm val)
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
    let ws = words input
    let cmd = head (ws ++ [""])
    case cmd of
        "exit" -> putStrLn "Goodbye!"
        "dump" -> do
            printMemory state
            repl state
        "poke" -> case ws of
            [_, addrStr, valStr] -> do
                let addr = read (ensureHex addrStr) :: Word32
                let val = read (ensureHex valStr) :: Word32
                let state' = writeMem state addr val
                repl state'
            _ -> putStrLn "Usage: poke <addr> <val>" >> repl state
        "step" -> case ws of
            [_, filename] -> stepFile filename state
            _ -> putStrLn "Usage: step <filename>" >> repl state
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

ensureHex :: String -> String
ensureHex s = if "0x" `isPrefixOf` s then s else "0x" ++ s

stepFile :: FilePath -> CPUState -> IO ()
stepFile path state = do
    content <- readFile path
    let lines' = lines content
    let parsed = map parseLineContent lines'
    case sequence parsed of
        Left err -> putStrLn $ "Error parsing file:\n" ++ err
        Right contents -> do
            let (instMap, labelMap) = assemble contents
            let resolvedInstMap = Map.map (resolveLabels labelMap) instMap
            stepProgram resolvedInstMap state

stepProgram :: Map.Map Word32 Instruction -> CPUState -> IO ()
stepProgram insts state = do
    let pcVal = getReg state PC
    case Map.lookup pcVal insts of
        Nothing -> putStrLn "Program Halted." >> repl state
        Just inst -> do
            putStrLn $ printf "[PC: 0x%08X] %s" pcVal (show inst)
            putStr "step> "
            hFlush stdout
            _ <- getLine -- Wait for Enter
            let state' = setReg state PC (pcVal + 4)
            let state'' = execute inst state'
            printRegisters state''
            stepProgram insts state''

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
