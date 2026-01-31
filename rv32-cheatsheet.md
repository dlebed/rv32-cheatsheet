# RISC-V 32-bit Architecture Cheatsheet

Target: RV32 I, M, C, Zce, Zbb, CMO, Smepmp

---

## 1. General-Purpose Registers (x0–x31)

| Reg   | ABI Name | Usage                        | Saver  |
|-------|----------|------------------------------|--------|
| x0    | zero     | Hardwired zero               | —      |
| x1    | ra       | Return address               | Caller |
| x2    | sp       | Stack pointer                | Callee |
| x3    | gp       | Global pointer               | —      |
| x4    | tp       | Thread pointer               | —      |
| x5    | t0       | Temp / alt link register     | Caller |
| x6–x7 | t1–t2   | Temporaries                  | Caller |
| x8    | s0/fp    | Saved register / frame ptr   | Callee |
| x9    | s1       | Saved register               | Callee |
| x10–x11 | a0–a1 | Function args / return values | Caller |
| x12–x17 | a2–a7 | Function arguments           | Caller |
| x18–x27 | s2–s11| Saved registers              | Callee |
| x28–x31 | t3–t6 | Temporaries                  | Caller |

**Compressed register set (3-bit encoding):** x8–x15 (s0–s1, a0–a5) — used by CIW/CL/CS/CA/CB formats.

---

## 2. Key CSR Registers

### Machine-Mode Setup

| Address | Name       | Description                                         |
|---------|------------|-----------------------------------------------------|
| 0x300   | mstatus    | Global interrupt enable (MIE bit[3]), prior IE (MPIE bit[7]), prior privilege (MPP bits[12:11]) |
| 0x301   | misa       | ISA extensions present (bit per letter)             |
| 0x304   | mie        | Interrupt enable: MSIE[3], MTIE[7], MEIE[11]        |
| 0x305   | mtvec      | Trap vector base. MODE[1:0]: 0=Direct, 1=Vectored. BASE[31:2] |
| 0x306   | mcounteren | Counter access enable for lower privilege            |
| 0x310   | mstatush   | Upper 32 bits of mstatus (RV32 only). MBE bit[5]   |

### Machine-Mode Trap Handling

| Address | Name     | Description                                          |
|---------|----------|------------------------------------------------------|
| 0x340   | mscratch | Scratch register for trap handlers                   |
| 0x341   | mepc     | Exception PC (saved on trap, restored by MRET)       |
| 0x342   | mcause   | Trap cause. bit[31]=interrupt flag, bits[30:0]=code  |
| 0x343   | mtval    | Trap value (bad address or bad instruction bits)     |
| 0x344   | mip      | Interrupt pending: MSIP[3], MTIP[7], MEIP[11]        |

### PMP Registers (base PMP)

| Address     | Name        | Description                                    |
|-------------|-------------|------------------------------------------------|
| 0x3A0–0x3A3 | pmpcfg0–3  | PMP config (4 entries per register, 8 bits each) |
| 0x3B0–0x3EF | pmpaddr0–63| PMP address (encodes bits[33:2] of phys addr)  |

**pmpcfg entry format (8 bits):**
```
  [7]   [6:5] [4:3] [2] [1] [0]
   L    (res)   A    X   W   R
```
- **A** (address matching): 0=OFF, 1=TOR, 2=NA4, 3=NAPOT
- **L** = Locked (persists until reset, enforced in M-mode too)
- TOR: region = `[pmpaddr[i-1] << 2, pmpaddr[i] << 2)`
- NAPOT: trailing ones in pmpaddr encode size. Size = 2^(trailing_ones + 3) bytes

### Smepmp (ePMP) — `mseccfg` CSR

| Address | Name     | Description                         |
|---------|----------|-------------------------------------|
| 0x747   | mseccfg  | PMP security configuration          |
| 0x757   | mseccfgh | Upper 32 bits (RV32 only)           |

**mseccfg fields:**
```
  [2]   [1]    [0]
  RLB   MMWP   MML
```
- **MML** (bit 0) — Machine Mode Lockdown (sticky). Reinterprets L bit:
  - L=1 → M-mode-only rule; L=0 → S/U-mode-only rule
  - RW=01 becomes "Shared Region" encoding
- **MMWP** (bit 1) — Machine Mode Whitelist Policy (sticky). Default-deny for M-mode on unmatched regions
- **RLB** (bit 2) — Rule Locking Bypass. Allows editing locked PMP entries (debug/boot use). Locks to 0 once any entry has L=1

### Zcmt Extension CSR

| Address | Name | Description                              |
|---------|------|------------------------------------------|
| 0x017   | jvt  | Jump Vector Table base (used by cm.jt/cm.jalt) |

---

## 3. 32-bit Instruction Encoding Formats

All 32-bit instructions: bits[1:0] = `11`.

```
R-type:  [31:25 funct7] [24:20 rs2] [19:15 rs1] [14:12 funct3] [11:7 rd] [6:0 opcode]
I-type:  [31:20 imm[11:0]]          [19:15 rs1] [14:12 funct3] [11:7 rd] [6:0 opcode]
S-type:  [31:25 imm[11:5]] [24:20 rs2] [19:15 rs1] [14:12 funct3] [11:7 imm[4:0]] [6:0 opcode]
B-type:  [31 imm[12]] [30:25 imm[10:5]] [24:20 rs2] [19:15 rs1] [14:12 f3] [11:8 imm[4:1]] [7 imm[11]] [6:0 op]
U-type:  [31:12 imm[31:12]]                                      [11:7 rd] [6:0 opcode]
J-type:  [31 imm[20]] [30:21 imm[10:1]] [20 imm[11]] [19:12 imm[19:12]]   [11:7 rd] [6:0 opcode]
```

**Immediate ranges:**
| Format | Immediate bits | Range / Alignment                  |
|--------|---------------|------------------------------------|
| I-type | 12-bit signed | −2048 to +2047                     |
| S-type | 12-bit signed | −2048 to +2047                     |
| B-type | 13-bit signed | −4096 to +4094, 2-byte aligned     |
| U-type | 20-bit upper  | bits[31:12], ±2^31 with AUIPC      |
| J-type | 21-bit signed | ±1 MiB, 2-byte aligned              |

Sign bit is always in instruction bit 31.

---

## 4. 16-bit Compressed Instruction Formats (C / Zca)

All 16-bit instructions: bits[1:0] ≠ `11`.

```
CR:  [15:12 funct4]  [11:7 rd/rs1]       [6:2 rs2]        [1:0 op]
CI:  [15:13 funct3]  [12 imm]  [11:7 rd/rs1]  [6:2 imm]   [1:0 op]
CSS: [15:13 funct3]  [12:7 imm]           [6:2 rs2]        [1:0 op]
CIW: [15:13 funct3]  [12:5 imm]                [4:2 rd']   [1:0 op]
CL:  [15:13 funct3]  [12:10 imm] [9:7 rs1']  [6 imm] [5:3 rd']  [1:0 op]
CS:  [15:13 funct3]  [12:10 imm] [9:7 rs1']  [6 imm] [5:3 rs2'] [1:0 op]
CA:  [15:10 funct6]  [9:7 rd'/rs1']  [6:5 funct2]  [4:2 rs2']   [1:0 op]
CB:  [15:13 funct3]  [12:10 offset] [9:7 rs1']  [6:2 offset]    [1:0 op]
CJ:  [15:13 funct3]  [12:2 jump_target]                          [1:0 op]
```

3-bit register fields (rd', rs1', rs2') encode **x8–x15** only.

---

## 5. RV32I Base Integer Instructions

### Arithmetic & Logic (R-type: opcode 0110011)
| Instruction | funct7  | funct3 | Operation           |
|-------------|---------|--------|---------------------|
| ADD         | 0000000 | 000    | rd = rs1 + rs2      |
| SUB         | 0100000 | 000    | rd = rs1 − rs2      |
| SLL         | 0000000 | 001    | rd = rs1 << rs2[4:0]|
| SLT         | 0000000 | 010    | rd = (rs1 < rs2) signed |
| SLTU        | 0000000 | 011    | rd = (rs1 < rs2) unsigned |
| XOR         | 0000000 | 100    | rd = rs1 ^ rs2      |
| SRL         | 0000000 | 101    | rd = rs1 >> rs2[4:0] (logical) |
| SRA         | 0100000 | 101    | rd = rs1 >> rs2[4:0] (arithmetic) |
| OR          | 0000000 | 110    | rd = rs1 \| rs2     |
| AND         | 0000000 | 111    | rd = rs1 & rs2      |

### Immediate Arithmetic (I-type: opcode 0010011)
| Instruction | imm[11:5] | funct3 | Operation              |
|-------------|-----------|--------|------------------------|
| ADDI        | —         | 000    | rd = rs1 + sext(imm)   |
| SLTI        | —         | 010    | rd = (rs1 < sext(imm)) signed |
| SLTIU       | —         | 011    | rd = (rs1 < sext(imm)) unsigned |
| XORI        | —         | 100    | rd = rs1 ^ sext(imm)   |
| ORI         | —         | 110    | rd = rs1 \| sext(imm)  |
| ANDI        | —         | 111    | rd = rs1 & sext(imm)   |
| SLLI        | 0000000   | 001    | rd = rs1 << imm[4:0]   |
| SRLI        | 0000000   | 101    | rd = rs1 >> imm[4:0] (logical) |
| SRAI        | 0100000   | 101    | rd = rs1 >> imm[4:0] (arith)  |

### Loads (I-type: opcode 0000011)
| Instruction | funct3 | Width            |
|-------------|--------|------------------|
| LB          | 000    | 8-bit, sign-ext  |
| LH          | 001    | 16-bit, sign-ext |
| LW          | 010    | 32-bit           |
| LBU         | 100    | 8-bit, zero-ext  |
| LHU         | 101    | 16-bit, zero-ext |

### Stores (S-type: opcode 0100011)
| Instruction | funct3 | Width  |
|-------------|--------|--------|
| SB          | 000    | 8-bit  |
| SH          | 001    | 16-bit |
| SW          | 010    | 32-bit |

### Branches (B-type: opcode 1100011)
| Instruction | funct3 | Condition        |
|-------------|--------|------------------|
| BEQ         | 000    | rs1 == rs2       |
| BNE         | 001    | rs1 != rs2       |
| BLT         | 100    | rs1 < rs2 signed |
| BGE         | 101    | rs1 >= rs2 signed|
| BLTU        | 110    | rs1 < rs2 unsigned |
| BGEU        | 111    | rs1 >= rs2 unsigned |

### Upper Immediate & Jumps
| Instruction | Type   | Opcode  | Operation                     |
|-------------|--------|---------|-------------------------------|
| LUI         | U-type | 0110111 | rd = imm << 12                |
| AUIPC       | U-type | 0010111 | rd = PC + (imm << 12)         |
| JAL         | J-type | 1101111 | rd = PC+4; PC += sext(imm)    |
| JALR        | I-type | 1100111 | rd = PC+4; PC = (rs1 + sext(imm)) & ~1 |

### System
| Instruction | Operation                           |
|-------------|-------------------------------------|
| ECALL       | Environment call                    |
| EBREAK      | Breakpoint                          |
| MRET        | Return from M-mode trap             |
| WFI         | Wait for interrupt                  |
| FENCE       | Memory ordering fence               |
| FENCE.I     | Instruction fetch fence (Zifencei)  |

### CSR Instructions (I-type: opcode 1110011)
| Instruction | funct3 | Operation                          |
|-------------|--------|------------------------------------|
| CSRRW       | 001    | rd = CSR; CSR = rs1                |
| CSRRS       | 010    | rd = CSR; CSR \|= rs1 (set bits)  |
| CSRRC       | 011    | rd = CSR; CSR &= ~rs1 (clear bits)|
| CSRRWI      | 101    | rd = CSR; CSR = zimm[4:0]          |
| CSRRSI      | 110    | rd = CSR; CSR \|= zimm[4:0]       |
| CSRRCI      | 111    | rd = CSR; CSR &= ~zimm[4:0]       |

---

## 6. M Extension — Integer Multiply/Divide

R-type, opcode 0110011, funct7 = 0000001.

| Instruction | funct3 | Operation                           |
|-------------|--------|-------------------------------------|
| MUL         | 000    | rd = (rs1 × rs2)[31:0]             |
| MULH        | 001    | rd = (rs1 × rs2)[63:32] (signed×signed) |
| MULHSU      | 010    | rd = (rs1 × rs2)[63:32] (signed×unsigned) |
| MULHU       | 011    | rd = (rs1 × rs2)[63:32] (unsigned×unsigned) |
| DIV         | 100    | rd = rs1 ÷ rs2 (signed)            |
| DIVU        | 101    | rd = rs1 ÷ rs2 (unsigned)          |
| REM         | 110    | rd = rs1 % rs2 (signed)            |
| REMU        | 111    | rd = rs1 % rs2 (unsigned)          |

---

## 7. C Extension — Compressed Instructions (16-bit)

### Quadrant 0 (op = 00)
| Instruction  | Format | Operation                             |
|--------------|--------|---------------------------------------|
| C.ADDI4SPN   | CIW    | rd' = sp + nzuimm (scaled ×4)        |
| C.LW         | CL     | rd' = M[rs1' + uimm] (32-bit load)   |
| C.SW         | CS     | M[rs1' + uimm] = rs2' (32-bit store) |

### Quadrant 1 (op = 01)
| Instruction  | Format | Operation                             |
|--------------|--------|---------------------------------------|
| C.NOP        | CI     | No operation                          |
| C.ADDI       | CI     | rd = rd + nzsimm (6-bit signed)       |
| C.JAL        | CJ     | ra = PC+2; PC += sext(imm) (RV32 only)|
| C.LI         | CI     | rd = sext(imm)                        |
| C.ADDI16SP   | CI     | sp = sp + nzsimm (scaled ×16)         |
| C.LUI        | CI     | rd = nzsimm << 12                     |
| C.SRLI       | CB     | rd' >>= shamt (logical)              |
| C.SRAI       | CB     | rd' >>= shamt (arithmetic)           |
| C.ANDI       | CB     | rd' &= sext(imm)                     |
| C.SUB        | CA     | rd' = rd' − rs2'                     |
| C.XOR        | CA     | rd' = rd' ^ rs2'                     |
| C.OR         | CA     | rd' = rd' \| rs2'                    |
| C.AND        | CA     | rd' = rd' & rs2'                     |
| C.J          | CJ     | PC += sext(imm)                       |
| C.BEQZ       | CB     | if (rs1' == 0) PC += sext(offset)     |
| C.BNEZ       | CB     | if (rs1' != 0) PC += sext(offset)     |

### Quadrant 2 (op = 10)
| Instruction  | Format | Operation                             |
|--------------|--------|---------------------------------------|
| C.SLLI       | CI     | rd <<= shamt                          |
| C.LWSP       | CI     | rd = M[sp + uimm] (32-bit, ×4 scaled)|
| C.JR         | CR     | PC = rs1                              |
| C.MV         | CR     | rd = rs2                              |
| C.EBREAK     | CR     | Breakpoint                            |
| C.JALR       | CR     | ra = PC+2; PC = rs1                   |
| C.ADD        | CR     | rd = rd + rs2                         |
| C.SWSP       | CSS    | M[sp + uimm] = rs2 (32-bit, ×4 scaled)|

---

## 8. Zce — Code Size Reduction (Zca + Zcb + Zcmp + Zcmt)

### Zcb — Compact Basic Operations (16-bit, x8–x15 only)

| Instruction | Operation                              |
|-------------|----------------------------------------|
| c.lbu       | rd' = zext(M8[rs1' + uimm])           |
| c.lhu       | rd' = zext(M16[rs1' + uimm])          |
| c.lh        | rd' = sext(M16[rs1' + uimm])          |
| c.sb        | M8[rs1' + uimm] = rs2'               |
| c.sh        | M16[rs1' + uimm] = rs2'              |
| c.zext.b    | rd' = rd'[7:0] (zero-extend byte)     |
| c.sext.b    | rd' = sext(rd'[7:0])                  |
| c.zext.h    | rd' = rd'[15:0] (zero-extend half)    |
| c.sext.h    | rd' = sext(rd'[15:0])                 |
| c.not       | rd' = ~rd'                            |
| c.mul       | rd' = rd' × rs2'                      |
| c.zext.w    | (RV64 only)                           |

### Zcmp — Push/Pop & Register Moves (16-bit)

| Instruction  | Operation                                                |
|--------------|----------------------------------------------------------|
| cm.push      | Push ra, {s0–sN} onto stack; sp −= stack_adj            |
| cm.pop       | Pop ra, {s0–sN} from stack; sp += stack_adj              |
| cm.popret    | Pop + return (= cm.pop + ret)                            |
| cm.popretz   | Pop + a0=0 + return                                      |
| cm.mva01s    | a0 = ssrc1, a1 = ssrc2 (from s0–s7)                     |
| cm.mvsa01    | sdst1 = a0, sdst2 = a1 (to s0–s7)                       |

`reg_list`: {ra}, {ra,s0}, {ra,s0-s1}, ... {ra,s0-s11} (12 valid lists, {ra,s0-s10} is excluded).
`stack_adj`: multiple of 16, encoding provides additional adjustment beyond register save area.

### Zcmt — Table Jump (16-bit)

| Instruction | Operation                                      |
|-------------|-------------------------------------------------|
| cm.jt       | PC = jvt_table[index] (index = 0–31)           |
| cm.jalt     | ra = PC+2; PC = jvt_table[index] (index = 32–255) |

Uses `jvt` CSR (0x017) as table base address. Table entries are 32-bit addresses.

---

## 9. Zbb — Basic Bit Manipulation

R-type / I-type, various funct7/funct3 encodings.

### Logical with Negate
| Instruction | Operation              |
|-------------|------------------------|
| ANDN        | rd = rs1 & ~rs2        |
| ORN         | rd = rs1 \| ~rs2      |
| XNOR        | rd = ~(rs1 ^ rs2)      |

### Count Leading/Trailing Zeros & Population Count
| Instruction | Operation                         |
|-------------|-----------------------------------|
| CLZ         | rd = count_leading_zeros(rs1)     |
| CTZ         | rd = count_trailing_zeros(rs1)    |
| CPOP        | rd = population_count(rs1)        |

### Min / Max
| Instruction | Operation                  |
|-------------|----------------------------|
| MIN         | rd = min(rs1, rs2) signed  |
| MAX         | rd = max(rs1, rs2) signed  |
| MINU        | rd = min(rs1, rs2) unsigned|
| MAXU        | rd = max(rs1, rs2) unsigned|

### Sign / Zero Extension
| Instruction | Operation                        |
|-------------|----------------------------------|
| SEXT.B      | rd = sign_extend(rs1[7:0])       |
| SEXT.H      | rd = sign_extend(rs1[15:0])      |
| ZEXT.H      | rd = zero_extend(rs1[15:0])      |

Note: SEXT.B/SEXT.H are pseudo-ops encoded as specific ADDI variants in the base ISA, but Zbb provides dedicated encodings (unary R-type with funct7 = 0110000, rs2 = 00100/00101).

### Bitwise Rotation
| Instruction | Operation                           |
|-------------|-------------------------------------|
| ROL         | rd = rotate_left(rs1, rs2[4:0])     |
| ROR         | rd = rotate_right(rs1, rs2[4:0])    |
| RORI        | rd = rotate_right(rs1, shamt[4:0])  |

### Byte-Reverse & OR-Combine
| Instruction | Operation                                   |
|-------------|---------------------------------------------|
| REV8        | rd = byte_reverse(rs1)                      |
| ORC.B       | rd = per-byte: 0xFF if any bit set, else 0  |

---

## 10. CMO — Cache Management Operations

### Zicbom — Cache Block Management

All use base register rs1 for address. No destination register. Encoded in MISC-MEM opcode space.

| Instruction   | Operation                                    |
|---------------|----------------------------------------------|
| CBO.INVAL     | Invalidate cache block containing M[rs1]     |
| CBO.CLEAN     | Clean (writeback) cache block containing M[rs1] |
| CBO.FLUSH     | Clean + invalidate cache block               |

### Zicboz — Cache Block Zero

| Instruction   | Operation                                    |
|---------------|----------------------------------------------|
| CBO.ZERO      | Zero entire cache block containing M[rs1]    |

### Zicbop — Cache Block Prefetch

Uses PREFETCH hint encoding with 12-bit signed offset (imm[11:0]) from rs1.

| Instruction   | Operation                                    |
|---------------|----------------------------------------------|
| PREFETCH.I    | Prefetch for instruction fetch               |
| PREFETCH.R    | Prefetch for data read                       |
| PREFETCH.W    | Prefetch for data write                      |

Cache block size is implementation-defined and discoverable at runtime.

---

## 11. Smepmp (ePMP) — Enhanced PMP Summary

### Modified pmpcfg Encoding When MML=1

| L | R | W | X | M-mode     | S/U-mode   | Description           |
|---|---|---|---|------------|------------|-----------------------|
| 0 | 0 | 0 | 0 | None       | None       | OFF                   |
| 0 | 0 | 0 | 1 | None       | X          | S/U exec-only         |
| 0 | 0 | 1 | 0 | None       | RW         | S/U Shared read/write |
| 0 | 0 | 1 | 1 | None       | RWX        | S/U full access       |
| 0 | 1 | 0 | 0 | None       | R          | S/U read-only         |
| 0 | 1 | 0 | 1 | None       | RX         | S/U read-exec         |
| 0 | 1 | 1 | 0 | R          | R          | Shared read-only      |
| 0 | 1 | 1 | 1 | RX         | RX         | Shared read-exec      |
| 1 | 0 | 0 | 0 | None       | None       | M-mode locked OFF     |
| 1 | 0 | 0 | 1 | X          | None       | M-mode exec-only      |
| 1 | 0 | 1 | 0 | RW         | None       | M-mode read/write     |
| 1 | 0 | 1 | 1 | RWX        | None       | M-mode full access    |
| 1 | 1 | 0 | 0 | R          | None       | M-mode read-only      |
| 1 | 1 | 0 | 1 | RX         | None       | M-mode read-exec      |
| 1 | 1 | 1 | 0 | R          | RW         | Shared: M=R, S/U=RW   |
| 1 | 1 | 1 | 1 | R          | R          | Shared read-only (locked) |

### Key Behavior Rules
- Without `mseccfg` writes, standard PMP behavior is preserved
- MML and MMWP are **sticky** (cannot be unset until reset)
- With MMWP=1: M-mode accesses to unmatched regions are **denied** (default-deny)
- With MML=1: M-mode can only execute from M-mode-only or locked shared regions
- RLB=1 allows editing locked entries (boot/debug only). RLB locks to 0 once any L=1 entry exists

---

## 12. Pseudoinstructions & Assembly Conventions

### 12.1 Pseudoinstructions — Register Operations

| Pseudoinstruction | Base Expansion | Notes |
|---|---|---|
| NOP | ADDI x0, x0, 0 | No operation |
| LI rd, imm | ADDI rd, x0, imm | imm in [−2048, 2047] |
| LI rd, imm | LUI rd, %hi(imm) + ADDI rd, rd, %lo(imm) | Outside 12-bit range |
| LA rd, symbol | AUIPC rd, %pcrel_hi(sym) + ADDI rd, rd, %pcrel_lo(label) | PC-relative (default) |
| LA rd, symbol | LUI rd, %hi(sym) + ADDI rd, rd, %lo(sym) | Absolute (norelax) |
| MV rd, rs | ADDI rd, rs, 0 | Copy register |
| NOT rd, rs | XORI rd, rs, -1 | Bitwise complement |
| NEG rd, rs | SUB rd, x0, rs | Two's complement negate |
| SEQZ rd, rs | SLTIU rd, rs, 1 | Set if == 0 |
| SNEZ rd, rs | SLTU rd, x0, rs | Set if != 0 |
| SLTZ rd, rs | SLT rd, rs, x0 | Set if < 0 |
| SGTZ rd, rs | SLT rd, x0, rs | Set if > 0 |

**Notes:** LI with bits[11:0]==0 uses only LUI. MV may compress to C.MV.

### 12.2 Pseudoinstructions — Branch Aliases

| Pseudoinstruction | Base Expansion |
|---|---|
| BEQZ rs, offset | BEQ rs, x0, offset |
| BNEZ rs, offset | BNE rs, x0, offset |
| BLEZ rs, offset | BGE x0, rs, offset |
| BGEZ rs, offset | BGE rs, x0, offset |
| BLTZ rs, offset | BLT rs, x0, offset |
| BGTZ rs, offset | BLT x0, rs, offset |
| BGT rs, rt, offset | BLT rt, rs, offset |
| BLE rs, rt, offset | BGE rt, rs, offset |
| BGTU rs, rt, offset | BLTU rt, rs, offset |
| BLEU rs, rt, offset | BGEU rt, rs, offset |

**Note:** Operand-swap aliases reuse existing branch opcodes by swapping rs1/rs2.

### 12.3 Pseudoinstructions — Jump & Call

| Pseudoinstruction | Base Expansion | Notes |
|---|---|---|
| J offset | JAL x0, offset | Unconditional jump |
| JR rs | JALR x0, rs, 0 | Jump register |
| RET | JALR x0, ra, 0 | Return from subroutine |
| CALL symbol | AUIPC ra, %pcrel_hi(sym) + JALR ra, ra, %pcrel_lo(label) | Far call |
| TAIL symbol | AUIPC t1, %pcrel_hi(sym) + JALR x0, t1, %pcrel_lo(label) | Far tail call |

**Notes:** CALL/TAIL are two-instruction sequences (AUIPC+JALR). TAIL uses t1 as scratch. Linker relaxation may reduce to single JAL.

### 12.4 Pseudoinstructions — CSR Shorthands

| Pseudoinstruction | Base Expansion | Notes |
|---|---|---|
| CSRR rd, csr | CSRRS rd, csr, x0 | Read CSR |
| CSRW csr, rs | CSRRW x0, csr, rs | Write CSR |
| CSRS csr, rs | CSRRS x0, csr, rs | Set bits in CSR |
| CSRC csr, rs | CSRRC x0, csr, rs | Clear bits in CSR |
| CSRWI csr, imm | CSRRWI x0, csr, imm | Write CSR (immediate) |
| CSRSI csr, imm | CSRRSI x0, csr, imm | Set bits (immediate) |
| CSRCI csr, imm | CSRRCI x0, csr, imm | Clear bits (immediate) |

**Note:** CSRRS/CSRRC with rs1=x0 are read-only. CSRRW/CSRRWI with rd=x0 discard old value but write still occurs.

### 12.5 Relocation Operators

| Operator | Meaning | Typical Pattern |
|---|---|---|
| %hi(sym) | Upper 20 bits of absolute address | LUI rd, %hi(sym) |
| %lo(sym) | Lower 12 bits of absolute address | ADDI rd, rd, %lo(sym) |
| %pcrel_hi(sym) | Upper 20 bits of PC-relative offset | AUIPC rd, %pcrel_hi(sym) |
| %pcrel_lo(label) | Lower 12 bits of PC-relative offset | ADDI rd, rd, %pcrel_lo(label) |

```
# Absolute address loading
  LUI   rd, %hi(symbol)
  ADDI  rd, rd, %lo(symbol)

# PC-relative address loading (default with relaxation)
label:
  AUIPC rd, %pcrel_hi(symbol)
  ADDI  rd, rd, %pcrel_lo(label)
```

**Note:** %hi adds +1 when bit[11] is set to compensate for ADDI sign-extension. %pcrel_lo references the AUIPC label, not the target symbol.

### 12.6 Common Assembler Directives

| Directive | Description |
|---|---|
| .text | Switch to text (code) section |
| .data | Switch to initialized data section |
| .bss | Switch to uninitialized data section |
| .section name | Switch to named section |
| .globl sym | Make symbol globally visible |
| .local sym | Mark symbol as local |
| .align n | Align next datum to 2^n byte boundary |
| .balign n | Align next datum to n byte boundary |
| .byte val | Emit 8-bit value(s) |
| .half val | Emit 16-bit value(s) |
| .word val | Emit 32-bit value(s) |
| .zero n | Emit n zero bytes |
| .string str / .asciz str | Emit NUL-terminated string |
| .equ sym, val / .set sym, val | Define symbolic constant |
| .option rvc | Enable compressed instruction emission |
| .option norvc | Disable compressed instruction emission |
| .option relax | Enable linker relaxation (default) |
| .option norelax | Disable linker relaxation |
| .type sym, @function/@object | Set ELF symbol type |
| .size sym, expr | Set ELF symbol size |

### 12.7 Calling Convention (RV32 ILP32)

#### Argument Passing

- **a0–a7** carry integer arguments; additional arguments spill to the stack
- 64-bit arguments use aligned even–odd register pairs (e.g., a0–a1, a2–a3)
- Return value in **a0** (or a0–a1 for 64-bit)
- Large structs returned via hidden pointer in **a0**
- **sp** must be 16-byte aligned at call boundaries

#### Prologue / Epilogue Templates

```
# Leaf function (no calls, no saved regs)
leaf:
    # ... body (use a0-a7, t0-t6 freely) ...
    ret

# Non-leaf function
non_leaf:
    addi  sp, sp, -FRAME       # allocate stack frame
    sw    ra, FRAME-4(sp)      # save return address
    sw    s0, FRAME-8(sp)      # save callee-saved regs
    sw    s1, FRAME-12(sp)
    # ... body ...
    lw    s1, FRAME-12(sp)     # restore callee-saved regs
    lw    s0, FRAME-8(sp)
    lw    ra, FRAME-4(sp)      # restore return address
    addi  sp, sp, FRAME        # deallocate stack frame
    ret
```

#### Stack Frame Layout

```
    ┌──────────────────────┐  ← Higher addresses
    │  Caller's frame      │
    ├──────────────────────┤
    │  Saved ra            │
    │  Saved s0–sN         │
    ├──────────────────────┤
    │  Local variables     │
    ├──────────────────────┤
    │  Outgoing args       │
    └──────────────────────┘  ← sp (16-byte aligned)
```

#### Register Convention Summary

| Registers | ABI Names | Role | Saver |
|---|---|---|---|
| x10–x17 | a0–a7 | Arguments / return values | Caller |
| x5–x7, x28–x31 | t0–t6 | Temporaries | Caller |
| x8–x9, x18–x27 | s0–s11 | Saved registers | Callee |
| x1 | ra | Return address | Caller |
| x2 | sp | Stack pointer | Callee |
| x3 | gp | Global pointer | — (not allocatable) |
| x4 | tp | Thread pointer | — (not allocatable) |

---

## 13. Interrupt Handling & CPU Behavior

### 13.1 Interrupt vs Exception Overview

- **Interrupt** (asynchronous): caused by external or timer event. `mcause[31]=1`. `mepc` = address of *next* instruction to execute.
- **Exception** (synchronous): caused by executing an instruction. `mcause[31]=0`. `mepc` = address of the *faulting* instruction.
- Interrupts are taken only between instructions when `mstatus.MIE=1`.
- **WFI** halts the hart until an interrupt is pending. May resume even if `mstatus.MIE=0` (implementation-defined).

### 13.2 mstatus Interrupt-Related Fields

```
  [31:13] [12:11] [10:8] [7]   [6:4] [3]   [2:0]
  (other)  MPP    (res)  MPIE  (res)  MIE   (res)
```

| Field | Bits | Description |
|-------|------|-------------|
| MIE   | [3]  | Machine Interrupt Enable. 1 = interrupts enabled. |
| MPIE  | [7]  | Previous MIE value (saved on trap entry, restored by MRET). |
| MPP   | [12:11] | Previous privilege mode (saved on trap entry). On M-only systems, hardwired to `2'b11`. |

**Notes:** MPIE/MPP form a one-level stack. On trap entry, MIE is pushed to MPIE and MIE is cleared. MRET reverses this.

### 13.3 mtvec Modes

```
  [31:2]  [1:0]
   BASE    MODE
```

| MODE | Value | Behavior |
|------|-------|----------|
| Direct   | 0 | All traps jump to `BASE`. |
| Vectored | 1 | Exceptions jump to `BASE`. Interrupts jump to `BASE + 4 * cause_code`. |

**Notes:** BASE must be 4-byte aligned (MODE=0) or aligned to the table size (MODE=1). In vectored mode, exceptions always go to `BASE`.

### 13.4 mcause Values

**Interrupts** (mcause[31] = 1):

| Code | Name | Source |
|------|------|--------|
| 3    | Machine Software Interrupt (MSI) | CLINT MSIP register |
| 7    | Machine Timer Interrupt (MTI) | CLINT mtime >= mtimecmp |
| 11   | Machine External Interrupt (MEI) | PLIC |
| >=16 | Platform-defined | Implementation-specific |

**Exceptions** (mcause[31] = 0):

| Code | Name | mtval content |
|------|------|---------------|
| 0  | Instruction address misaligned | Faulting address |
| 1  | Instruction access fault | Faulting address |
| 2  | Illegal instruction | Faulting instruction bits |
| 3  | Breakpoint (EBREAK) | Faulting PC |
| 4  | Load address misaligned | Faulting address |
| 5  | Load access fault | Faulting address |
| 6  | Store/AMO address misaligned | Faulting address |
| 7  | Store/AMO access fault | Faulting address |
| 8  | Environment call from U-mode | 0 |
| 9  | (Reserved) | — |
| 10 | (Reserved) | — |
| 11 | Environment call from M-mode | 0 |
| 12 | Instruction page fault | Faulting address |
| 13 | Load page fault | Faulting address |
| 14 | (Reserved) | — |
| 15 | Store/AMO page fault | Faulting address |

**Notes:** On M-only systems, codes 8–10 and 12–15 may not be generated. `mtval` may be zero if not supported.

### 13.5 mie / mip Bit Layout

```
  [31:12]     [11]      [10:8] [7]      [6:4] [3]      [2:0]
  (platform)  MEIE/MEIP (res)  MTIE/MTIP (res) MSIE/MSIP (res)
```

| Bit | mie name | mip name | Source |
|-----|----------|----------|--------|
| 3   | MSIE     | MSIP     | CLINT software interrupt register |
| 7   | MTIE     | MTIP     | CLINT timer (mtime >= mtimecmp) |
| 11  | MEIE     | MEIP     | PLIC external interrupt |

**Interrupt-taken condition:** `mstatus.MIE=1 AND mie[N]=1 AND mip[N]=1`

### 13.6 CPU Behavior on Trap Entry

Hardware-atomic sequence when a trap is taken:

1. `mepc` <- PC (faulting instruction for exceptions, next instruction for interrupts)
2. `mcause` <- cause code (with bit[31] set for interrupts)
3. `mtval` <- trap value (bad address, bad instruction bits, or 0)
4. `mstatus.MPIE` <- `mstatus.MIE`
5. `mstatus.MIE` <- 0 (interrupts disabled)
6. `mstatus.MPP` <- current privilege mode
7. PC <- `mtvec` target (BASE for direct mode; BASE + 4*cause for vectored interrupts)

### 13.7 CPU Behavior on MRET

1. PC <- `mepc`
2. `mstatus.MIE` <- `mstatus.MPIE`
3. `mstatus.MPIE` <- 1
4. Privilege mode <- `mstatus.MPP`
5. `mstatus.MPP` <- least-privileged supported mode (M on M-only systems)

### 13.8 Interrupt Priority

| Priority | Interrupt | mcause code |
|----------|-----------|-------------|
| Highest  | Machine External (MEI) | 11 |
|          | Machine Software (MSI) | 3  |
| Lowest   | Machine Timer (MTI)    | 7  |

**Notes:** This is the default spec priority when multiple interrupts are pending simultaneously. Synchronous exceptions take precedence over asynchronous interrupts. PLIC provides sub-priority among external interrupt sources.

### 13.9 Typical ISR Prologue/Epilogue

```
isr_entry:
    csrrw sp, mscratch, sp     # swap sp <-> mscratch (switch to ISR stack)
    addi  sp, sp, -64          # allocate frame (adjust size as needed)
    sw    ra,  60(sp)
    sw    t0,  56(sp)
    sw    t1,  52(sp)
    sw    t2,  48(sp)
    sw    a0,  44(sp)
    sw    a1,  40(sp)
    # ... remaining caller-saved: a2-a7, t3-t6 ...

    csrr  a0, mcause           # read trap cause
    csrr  a1, mepc             # read exception PC
    sw    a1, 0(sp)            # save mepc (needed if nested)

    call  trap_dispatch         # C handler: trap_dispatch(mcause, mepc)

    lw    a1, 0(sp)            # restore mepc
    csrw  mepc, a1
    # ... restore a2-a7, t3-t6 ...
    lw    a1,  40(sp)
    lw    a0,  44(sp)
    lw    t2,  48(sp)
    lw    t1,  52(sp)
    lw    t0,  56(sp)
    lw    ra,  60(sp)
    addi  sp, sp, 64           # deallocate frame
    csrrw sp, mscratch, sp     # restore original sp
    mret
```

**Notes:**
- Only caller-saved registers need saving if the ISR calls C functions.
- `mscratch` technique: at boot, store ISR stack pointer in `mscratch`. The double-swap restores the original `sp` on exit.
- For nested interrupts: save `mepc`/`mcause`, re-enable `mstatus.MIE`, then call handler.
- Zcmp `cm.push`/`cm.popret` can replace manual save/restore of `ra, s0–sN` in the C handler itself.

---

## Quick Reference: Encoding Summary

| Extension | Instruction Width | Key Opcodes / Space                    |
|-----------|-------------------|----------------------------------------|
| RV32I     | 32-bit            | bits[1:0]=11, standard opcodes         |
| M         | 32-bit            | opcode=0110011, funct7=0000001         |
| C (Zca)   | 16-bit            | bits[1:0]≠11, quadrants 00/01/10       |
| Zcb       | 16-bit            | Reuses CA/CL/CS format slots           |
| Zcmp      | 16-bit            | Reuses c.fsdsp encoding space          |
| Zcmt      | 16-bit            | Reuses c.fsdsp encoding space          |
| Zbb       | 32-bit            | Various funct7 under OP/OP-IMM        |
| Zicbom    | 32-bit            | MISC-MEM opcode (0001111)              |
| Zicboz    | 32-bit            | MISC-MEM opcode (0001111)              |
| Zicbop    | 32-bit            | ORI opcode space (prefetch hints)      |
| Smepmp    | — (CSR-only)      | mseccfg @ 0x747, mseccfgh @ 0x757     |
