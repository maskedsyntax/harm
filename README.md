# HARM: ARM7 Emulator

A CLI-first ARM7 instruction set emulator implemented in Haskell.

## Features

- **Instruction Support**:
  - Data Processing: `MOV`, `MVN`, `ADD`, `SUB`, `RSB`, `ADC`, `SBC`, `RSC`, `MUL`, `AND`, `ORR`, `EOR`, `BIC`
  - Comparison & Test: `CMP`, `CMN`, `TST`, `TEQ`
  - Memory: `LDR`, `STR`, `LDRB`, `STRB`, `LDRH`, `STRH`
  - Block Transfer: `LDM`, `STM` (Supports IA, IB, DA, DB modes and write-back `!`)
  - Control Flow: `B`, `BL` (Branch with Link)
- **Operand Shifting**: Full support for `LSL`, `LSR`, `ASR`, and `ROR` on the second operand (immediate or register-based).
- **Pseudo-instructions**: Supports `ADR` and `LDR Rd, =value`.
- **Condition Codes**: Full support for all ARM condition codes (EQ, NE, CS, CC, MI, PL, VS, VC, HI, LS, GE, LT, GT, LE, AL).
- **CPSR Flags**: Accurate simulation of Negative (N), Zero (Z), Carry (C), and Overflow (V) flags.
- **I/O Simulation**: Virtual UART mapped to `0x10000000`. Writing to this address outputs characters to the terminal.
- **Debugging**: Support for breakpoints and instruction-level stepping.

## Usage

### Prerequisites

- GHC 9.4.7 or later
- Cabal 3.8 or later

### Build and Test

```bash
cabal build
cabal test
```

### Interactive REPL

```bash
cabal run harm
```

REPL Commands:
- `exit`: Terminate session.
- `dump`: Display all non-zero memory.
- `poke <addr> <val>`: Manually write a value to memory.
- `break <addr>`: Set a breakpoint at a specific address.
- `step <filename>`: Execute a file one instruction at a time.
- `<instruction>`: Execute a single ARM instruction (e.g., `MOV R1, #10`).

### File Execution

```bash
cabal run harm -- filename.asm
```

## Assembly Syntax

- **Registers**: `R0-R15`, `SP`, `LR`, `PC`. Case-insensitive.
- **Labels**: Defined with a colon (e.g., `loop:`).
- **Immediates**: Prefixed with `#` (e.g., `#42`, `#0x2A`).
- **Register Lists**: Used in LDM/STM (e.g., `{R0-R4, R12}`).
- **Comments**: Lines starting with `;`.
