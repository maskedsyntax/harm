module Parser where

import Text.Megaparsec hiding (label)
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
    [ string' "R0"  >> return R0,  string' "R1"  >> return R1
    , string' "R2"  >> return R2,  string' "R3"  >> return R3
    , string' "R4"  >> return R4,  string' "R5"  >> return R5
    , string' "R6"  >> return R6,  string' "R7"  >> return R7
    , string' "R8"  >> return R8,  string' "R9"  >> return R9
    , string' "R10" >> return R10, string' "R11" >> return R11
    , string' "R12" >> return R12, string' "R13" >> return R13
    , string' "R14" >> return R14, string' "R15" >> return R15
    , string' "SP"  >> return SP,  string' "LR"  >> return LR
    , string' "PC"  >> return PC
    ]

-- | Parse an operand
pOperand :: Parser Operand
pOperand = choice
    [ Imm <$> (symbol "#" >> pHexOrDec)
    , try pRegShift
    ]

pRegShift :: Parser Operand
pRegShift = do
    r <- pRegister
    shift <- optional (symbol "," >> pShift)
    return $ RegShift r shift

pShift :: Parser Shift
pShift = choice
    [ try (string' "LSL") >> sc >> (LSL <$> pShiftValue)
    , try (string' "LSR") >> sc >> (LSR <$> pShiftValue)
    , try (string' "ASR") >> sc >> (ASR <$> pShiftValue)
    , try (string' "ROR") >> sc >> (ROR <$> pShiftValue)
    ]

pShiftValue :: Parser ShiftValue
pShiftValue = choice
    [ ShiftImm <$> (symbol "#" >> pHexOrDec)
    , ShiftReg <$> pRegister
    ]

pHexOrDec :: Parser Word32
pHexOrDec = try (string "0x" >> L.hexadecimal) <|> L.decimal

-- | Parse a condition code
pCondition :: Parser Condition
pCondition = choice
    [ try (string' "EQ") >> return EQ'
    , try (string' "NE") >> return NE
    , try (string' "CS") >> return CS
    , try (string' "CC") >> return CC
    , try (string' "MI") >> return MI
    , try (string' "PL") >> return PL
    , try (string' "VS") >> return VS
    , try (string' "VC") >> return VC
    , try (string' "HI") >> return HI
    , try (string' "LS") >> return LS
    , try (string' "GE") >> return GE
    , try (string' "LT") >> return LT'
    , try (string' "GT") >> return GT'
    , try (string' "LE") >> return LE
    , return AL
    ]

-- | Parse a mnemonic, optional condition, and optional S bit
pMnemonicFull :: String -> Parser (Condition, Bool)
pMnemonicFull name = lexeme $ do
    _ <- string' name
    cond <- pCondition
    s <- (char' 'S' >> return True) <|> return False
    return (cond, s)

-- | Parse a MOV instruction
pMov :: Parser Instruction
pMov = do
    (cond, s) <- pMnemonicFull "MOV"
    dst <- pRegister
    _ <- symbol ","
    src <- pOperand
    return $ MOV cond s dst src

-- | Parse a MVN instruction
pMvn :: Parser Instruction
pMvn = do
    (cond, s) <- pMnemonicFull "MVN"
    dst <- pRegister
    _ <- symbol ","
    src <- pOperand
    return $ MVN cond s dst src

-- | Parse arithmetic/logic instructions with 3 operands
pALU3 :: String -> (Condition -> Bool -> Register -> Register -> Operand -> Instruction) -> Parser Instruction
pALU3 name constructor = do
    (cond, s) <- pMnemonicFull name
    dst <- pRegister
    _ <- symbol ","
    src1 <- pRegister
    _ <- symbol ","
    src2 <- pOperand
    return $ constructor cond s dst src1 src2

pAdd, pSub, pAnd, pOrr, pEor, pBic, pRsb :: Parser Instruction
pAdd = pALU3 "ADD" ADD
pSub = pALU3 "SUB" SUB
pAnd = pALU3 "AND" AND
pOrr = pALU3 "ORR" ORR
pEor = pALU3 "EOR" EOR
pBic = pALU3 "BIC" BIC
pRsb = pALU3 "RSB" RSB

-- | Parse MUL instruction
pMul :: Parser Instruction
pMul = do
    (cond, s) <- pMnemonicFull "MUL"
    dst <- pRegister
    _ <- symbol ","
    src1 <- pRegister
    _ <- symbol ","
    src2 <- pRegister
    return $ MUL cond s dst src1 src2

-- | Parse CMP instruction
pCmp :: Parser Instruction
pCmp = do
    _ <- string' "CMP"
    cond <- pCondition
    _ <- sc
    src1 <- pRegister
    _ <- symbol ","
    src2 <- pOperand
    return $ CMP cond src1 src2

-- | Parse a label definition (e.g., "loop:")
pLabelDef :: Parser String
pLabelDef = lexeme $ do
    name <- some (alphaNumChar <|> char '_')
    _ <- char' ':'
    return name

-- | Parse B instruction target
pTarget :: Parser Target
pTarget = choice
    [ try (ImmAddr <$> pHexOrDec)
    , TLabel <$> some (alphaNumChar <|> char '_')
    ]

-- | Parse B instruction
pBranch :: Parser Instruction
pBranch = do
    _ <- string' "B"
    cond <- pCondition
    _ <- sc
    target <- pTarget
    return $ B cond target

-- | Parse LDR/STR instructions
pLdrStr :: String -> (Condition -> Register -> Register -> Instruction) -> Parser Instruction
pLdrStr name constructor = do
    _ <- string' name
    cond <- pCondition
    _ <- sc
    dst <- pRegister
    _ <- symbol ","
    _ <- symbol "["
    src <- pRegister
    _ <- symbol "]"
    return $ constructor cond dst src

pLdr, pStr :: Parser Instruction
pLdr = pLdrStr "LDR" LDR
pStr = pLdrStr "STR" STR

-- | Parse a register list like {R0, R1-R3, R5}
pRegList :: Parser [Register]
pRegList = symbol "{" >> (concat <$> sepBy pRegRange (symbol ",")) <* symbol "}"
  where
    pRegRange = do
        start <- pRegister
        end <- optional (symbol "-" >> pRegister)
        case end of
            Nothing -> return [start]
            Just e  -> return [start .. e]

-- | Parse LDM/STM
pBlockTrans :: String -> (Condition -> AddressingMode -> Register -> Bool -> [Register] -> Instruction) -> Parser Instruction
pBlockTrans name constructor = do
    _ <- string' name
    mode <- pAddrMode
    cond <- pCondition
    _ <- sc
    base <- pRegister
    writeback <- (symbol "!" >> return True) <|> return False
    _ <- symbol ","
    regs <- pRegList
    return $ constructor cond mode base writeback regs

pAddrMode :: Parser AddressingMode
pAddrMode = choice
    [ try (string' "IA") >> return IA
    , try (string' "IB") >> return IB
    , try (string' "DA") >> return DA
    , try (string' "DB") >> return DB
    , return IA -- Default to Increment After
    ]

pLdm, pStm :: Parser Instruction
pLdm = pBlockTrans "LDM" LDM
pStm = pBlockTrans "STM" STM

-- | Simple parser for a single instruction
pInstruction :: Parser Instruction
pInstruction = choice 
    [ try pMov, try pMvn, try pAdd, try pSub, try pAnd, try pOrr
    , try pEor, try pBic, try pRsb, try pMul, try pCmp
    , try pLdr, try pStr, try pLdm, try pStm, try pBranch
    ]

-- | Either an instruction or a label definition
data LineContent = LInstruction Instruction | LLabel String deriving (Show, Eq)

pLine :: Parser (Maybe LineContent)
pLine = choice
    [ Just . LLabel <$> try pLabelDef
    , Just . LInstruction <$> try pInstruction
    , return Nothing
    ]

parseLineContent :: String -> Either String (Maybe LineContent)
parseLineContent input = case parse (sc >> pLine <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right val -> Right val

parseLine :: String -> Either String (Maybe Instruction)
parseLine input = case parse (sc >> optional pInstruction <* eof) "" input of
    Left err -> Left (errorBundlePretty err)
    Right val -> Right val
