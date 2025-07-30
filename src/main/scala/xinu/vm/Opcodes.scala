package xinu.vm

object Opcodes {


  /*
   * Note: This project uses AT&T syntax
   * Instructions like MOV r/m32, r32 and others that require a ModR/M byte share the same base opcode (e.g., 0x89),
   * but the ModR/M byte defines which registers or memory locations are used.
   * For instructions with opcode extensions like 0x81 /0 or 0xF7 /3, the /n is part of the ModR/M byte and we’ll need
   * to decode that separately.
   */

  // Data Movement
  val MOV_IMM32_EAX    = 0xB8  // MOV imm32, EAX (base opcode)
  val MOV_IMM32_ECX    = 0xB9
  val MOV_IMM32_EDX    = 0xBA
  val MOV_IMM32_EBX    = 0xBB
  val MOV_IMM32_ESP    = 0xBC
  val MOV_IMM32_EBP    = 0xBD
  val MOV_IMM32_ESI    = 0xBE
  val MOV_IMM32_EDI    = 0xBF

  val MOV_RM32_R32     = 0x89  // MOV r/m32, r32 (with ModR/M byte)
  val MOV_R32_RM32     = 0x8B  // MOV r32, r/m32 (with ModR/M byte)
  val LEA_M_R32        = 0x8D  // LEA m, r32

  // Arithmetic
  val ADD_IMM32_EAX    = 0x05
  val ADD_R32_RM32     = 0x01
  val ADD_RM32_R32     = 0x03
  val ADD_IMM32_RM32   = 0x81  // with /0 in ModR/M byte

  val SUB_IMM32_EAX    = 0x2D
  val SUB_R32_RM32     = 0x29
  val SUB_RM32_R32     = 0x2B
  val SUB_IMM32_RM32   = 0x81  // with /5 in ModR/M byte

  val INC_R32_BASE     = 0x40  // INC r32 = 0x40 + rd
  val DEC_R32_BASE     = 0x48  // DEC r32 = 0x48 + rd

  val NEG_RM32         = 0xF7  // with /3 in ModR/M byte

  val MUL_RM32         = 0xF7  // with /4 in ModR/M byte
  val IMUL_RM32        = 0xF7  // with /5 in ModR/M byte
  val DIV_RM32         = 0xF7  // with /6 in ModR/M byte
  val IDIV_RM32        = 0xF7  // with /7 in ModR/M byte

  // Logic & Bitwise
  val AND_RM32_R32     = 0x21
  val AND_R32_RM32     = 0x23
  val AND_RM32_IMM32   = 0x81  // with /4 in ModR/M byte

  val OR_RM32_R32      = 0x09
  val OR_R32_RM32      = 0x0B
  val OR_RM32_IMM32    = 0x81  // with /1 in ModR/M byte

  val XOR_RM32_R32     = 0x31
  val XOR_R32_RM32     = 0x33
  val XOR_RM32_IMM32   = 0x81  // with /6 in ModR/M byte

  val NOT_RM32         = 0xF7  // with /2 in ModR/M byte

  val SHL_RM32_IMM8    = 0xC1  // with /4 in ModR/M byte
  val SHR_RM32_IMM8    = 0xC1  // with /5 in ModR/M byte

  // Control Flow
  val JMP_REL8         = 0xEB
  val JMP_REL32        = 0xE9

  val JE_REL8          = 0x74
  val JE_REL32_1       = 0x0F
  val JE_REL32_2       = 0x84

  val JNE_REL8         = 0x75
  val JNE_REL32_1      = 0x0F
  val JNE_REL32_2      = 0x85

  val JG_REL8          = 0x7F
  val JG_REL32_1       = 0x0F
  val JG_REL32_2       = 0x8F

  val JL_REL8          = 0x7C
  val JL_REL32_1       = 0x0F
  val JL_REL32_2       = 0x8C

  val CALL_REL32       = 0xE8
  val RET              = 0xC3

  // Stack Operations
  val PUSH_R32_BASE    = 0x50  // PUSH r32 = 0x50 + rd
  val POP_R32_BASE     = 0x58  // POP r32 = 0x58 + rd

  // Comparison
  val CMP_RM32_IMM32   = 0x81  // with /7 in ModR/M byte
  val CMP_RM32_R32     = 0x39
  val CMP_R32_RM32     = 0x3B

  /* ModRM */

  //Mod
  val MOD_IND_ADDR_NO_DISP = 0b00
  val MOD_IND_ADDR_8_DISP  = 0b01
  val MOD_IND_ADDR_32_DISP = 0b10
  val MOD_REG_DIRECT       = 0b11

  //RM
  val RM_SIB_BYTE          = 0b100
  val RM_ABSOLUTE_ADDRESS  = 0b101

  /* SIB */
  val SIB_INDEX_DISABLED   = 0b100
  val SIB_BASE_DISABLED    = 0b101

}
