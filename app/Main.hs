module Main where

import ARM7
import Parser
import Instruction

main :: IO ()
main = do
    putStrLn "Initializing HARM (Haskell ARM7 Emulator)..."
    let cpu = initCPU
    print cpu

    let line = "MOV R1, #42"
    putStrLn $ "\nParsing line: " ++ line
    case parseLine line of
        Left err -> putStrLn $ "Error: " ++ err
        Right instr -> do
            print instr
            -- In future we will implement: execute instr cpu
            putStrLn "Instruction parsed successfully!"
