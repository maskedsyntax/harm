module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Word (Word32)
import ARM7
import Instruction

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parse a register
pRegister :: Parser Register
pRegister = lexeme $ choice
    [ symbol "R0"  >> return R0,  symbol "R1"  >> return R1
    , symbol "R2"  >> return R2,  symbol "R3"  >> return R3
    , symbol "R4"  >> return R4,  symbol "R5"  >> return R5
    , symbol "R6"  >> return R6,  symbol "R7"  >> return R7
    , symbol "R8"  >> return R8,  symbol "R9"  >> return R9
    , symbol "R10" >> return R10, symbol "R11" >> return R11
    , symbol "R12" >> return R12, symbol "R13" >> return R13
    , symbol "R14" >> return R14, symbol "R15" >> return R15
    , symbol "SP"  >> return SP,  symbol "LR"  >> return LR
    , symbol "PC"  >> return PC
    ]

-- | Parse an operand
pOperand :: Parser Operand
pOperand = choice
    [ Imm <$> (symbol "#" >> L.decimal)
    , Reg <$> pRegister
    ]

-- | Parse a MOV instruction
pMov :: Parser Instruction
pMov = do
    _ <- symbol "MOV"
    dst <- pRegister
    _ <- symbol ","
    src <- pOperand
    return $ MOV AL dst src

-- | Simple parser for a single instruction
pInstruction :: Parser Instruction
pInstruction = choice [try pMov]

parseLine :: String -> Either String Instruction
parseLine input = case parse (sc >> pInstruction <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right val -> Right val
