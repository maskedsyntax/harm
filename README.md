# HARM: ARM7 Emulator

A CLI-first ARM7 instruction set emulator implemented in Haskell.

## Features

- **Instruction Support**:
  - Data Processing: `MOV`, `MVN`, `ADD`, `SUB`, `RSB`, `MUL`, `AND`, `ORR`, `EOR`, `BIC`
  - Comparison: `CMP`
  - Memory: `LDR`, `STR` (Base register addressing)
  - Control Flow: `B` (Branching with conditional support)
- **Condition Codes**: Supports all standard ARM condition codes (e.g., `EQ`, `NE`, `LT`, `GT`, `AL`).
- **CPSR Flags**: Updates Zero and Negative flags via the `S` mnemonic suffix (e.g., `ADDS`).
- **Number Formats**: Supports both decimal and hexadecimal (`0x`) literals.

## Usage

### Prerequisites

- GHC 9.4.7 or later
- Cabal 3.8 or later

### Build

```bash
cabal build
```

### Interactive REPL

Launch the emulator in interactive mode to execute instructions line-by-line:

```bash
cabal run harm
```

Commands available in REPL:
- `exit`: Terminate the session.
- `dump`: Display current memory contents.
- Any valid ARM instruction (e.g., `MOV R1, #10`).

### File Execution

Run an assembly file sequentially:

```bash
cabal run harm -- path/to/file.asm
```

The emulator maps instructions to addresses starting at `0x0` with 4-byte increments. Branch targets use these absolute addresses.

## Assembly Syntax

- **Registers**: `R0` through `R15`, `SP`, `LR`, `PC`. Case-insensitive.
- **Immediate Values**: Prefixed with `#` (e.g., `#42`, `#0x2A`).
- **Memory**: Square bracket notation (e.g., `LDR R0, [R1]`).
- **Comments**: Lines starting with `;`.
