package xinu.vm

object Opcodes {

  /* NOTE: This project uses AT&T syntax */

  // Data Movement
  val MOV_IMM32_REG_BASE    = 0xB8
  val MOV_R32_RM32          = 0x89
  val MOV_RM32_R32          = 0x8B
  val MOV_EAX_MOFFS32       = 0xA3
  val MOVSX_RM8_R32         = 0x0F
  val MOVSX_RM8_R32_SEC     = 0xBE
  val MOVZX_RM8_R32         = 0x0F
  val MOVZX_RM8_R32_SEC     = 0xB6
  val LEA_M_R32             = 0x8D

  val CWTL                  = 0x98   // cwde (cbw/cwde/cdqe family)
  val CLTD                  = 0x99   // cdq

  // Arithmetic
  val ADD_IMM32_EAX         = 0x05
  val ADD_R32_RM32          = 0x01
  val ADD_RM32_R32          = 0x03
  val SUB_IMM32_EAX         = 0x2D
  val SUB_R32_RM32          = 0x29
  val SUB_RM32_R32          = 0x2B
  val INC_R32_BASE          = 0x40  // INC r32 = 0x40 + rd
  val DEC_R32_BASE          = 0x48  // DEC r32 = 0x48 + rd
  val IMUL_RM32_IMM         = 0x69

  /* 0F Prefix Group */
  val IMUL_RM32_RM          = 0x0F
  val IMUL_RM32_RM_SEC      = 0xAF

  // Logic & Bitwise
  val AND_R32_RM32          = 0x21
  val AND_RM32_R32          = 0x23
  val AND_IMM32_EAX         = 0x25
  val OR_R32_RM32           = 0x09
  val OR_RM32_R32           = 0x0B
  val XOR_R32_RM32          = 0x31
  val XOR_RM32_R32          = 0x33

  val TEST_RM32_R32         = 0x85

  // Control Flow
  val JMP_REL8              = 0xEB
  val JMP_REL32             = 0xE9
  val JE_REL8               = 0x74
  val JE_REL32              = 0x0F
  val JE_REL32_SEC          = 0x84
  val JNE_REL8              = 0x75
  val JNE_REL32             = 0x0F
  val JNE_REL32_SEC         = 0x85
  val JG_REL8               = 0x7F
  val JG_REL32              = 0x0F
  val JG_REL32_SEC          = 0x8F
  val JGE_REL8              = 0x7D
  val JGE_REL32             = 0x0F
  val JGE_REL32_SEC         = 0x8D
  val JLE_REL8              = 0x7E
  val JLE_REL32             = 0x0F
  val JLE_REL32_SEC         = 0x8E
  val JL_REL8               = 0x7C
  val JL_REL32              = 0x0F
  val JL_REL32_SEC          = 0x8C
  val CALL_REL32            = 0xE8
  val RET                   = 0xC3

  val LOOP                  = 0xE2
  val LOOPE                 = 0xE1
  val LOOPNE                = 0xE0
  val ENTER                 = 0xC8
  val LEAVE                 = 0xC9

  // Stack Operations
  val PUSH_R32_BASE         = 0x50  // PUSH r32 = 0x50 + rd
  val PUSH_IMM32            = 0x68
  val PUSH_IMM8             = 0x6A
  val POP_R32_BASE          = 0x58  // POP r32 = 0x58 + rd

  val PUSHA                 = 0x60
  val POPA                  = 0x61
  val PUSHF                 = 0x9C
  val POPF                  = 0x9D

  // Comparison
  val CMP_R32_RM32          = 0x39
  val CMP_RM32_R32          = 0x3B
  val CMP_IMM32_EAX         = 0x3D

  val SETO                  = 0x0F
  val SETO_SEC              = 0x90
  val SETNO                 = 0x0F
  val SETNO_SEC             = 0x91
  val SETNAE                = 0x0F
  val SETNAE_SEC            = 0x92
  val SETNB                 = 0x0F
  val SETNB_SEC             = 0x93
  val SETE                  = 0x0F
  val SETE_SEC              = 0x94
  val SETNE                 = 0x0F
  val SETNE_SEC             = 0x95
  val SETNA                 = 0x0F
  val SETNA_SEC             = 0x96
  val SETNBE                = 0x0F
  val SETNBE_SEC            = 0x97
  val SETS                  = 0x0F
  val SETS_SEC              = 0x98
  val SETNS                 = 0x0F
  val SETNS_SEC             = 0x99

  val XCHG_R32_RM32         = 0x87
  val XCHG_EAX_BASE         = 0x91
  val NOP                   = 0x90

  val REPNE                 = 0xF2
  val REP                   = 0xF3

  val MOVSB                 = 0xA4
  val MOVSD                 = 0xA5
  val STOSB                 = 0xAA
  val STOSD                 = 0xAB
  val LODSB                 = 0xAC
  val LODSD                 = 0xAD
  val SCASB                 = 0xAE
  val SCASD                 = 0xAF
  val CMPSB                 = 0xA6
  val CMPSD                 = 0xA7

  /* F7 Prefix */
  val TEST_IMM32_RM32       = 0xF7
  val TEST_IMM32_RM32_SEC   = 0
  val NOT_RM32              = 0xF7
  val NOT_RM32_SEC          = 2
  val NEG_RM32              = 0xF7
  val NEG_RM32_SEC          = 3
  val MUL_RM32              = 0xF7
  val MUL_RM32_SEC          = 4
  val IMUL_RM32             = 0xF7
  val IMUL_RM32_SEC         = 5
  val DIV_RM32              = 0xF7
  val DIV_RM32_SEC          = 6
  val IDIV_RM32             = 0xF7
  val IDIV_RM32_SEC         = 7

  /* 81 Prefix */
  val CMP_IMM32_RM32        = 0x81
  val CMP_IMM32_RM32_SEC    = 7
  val XOR_IMM32_RM32        = 0x81
  val XOR_IMM32_RM32_SEC    = 6
  val OR_IMM32_RM32         = 0x81
  val OR_IMM32_RM32_SEC     = 1
  val AND_IMM32_RM32        = 0x81
  val AND_IMM32_RM32_SEC    = 4
  val SUB_IMM32_RM32        = 0x81
  val SUB_IMM32_RM32_SEC    = 5
  val ADD_IMM32_RM32        = 0x81
  val ADD_IMM32_RM32_SEC    = 0

  /* C1 Prefix */
  val SAR_IMM8_RM32         = 0xC1
  val SAR_IMM8_RM32_SEC     = 7
  val SHR_IMM8_RM32         = 0xC1
  val SHR_IMM8_RM32_SEC     = 5
  val SHL_IMM8_RM32         = 0xC1
  val SHL_IMM8_RM32_SEC     = 4

  val ROL_IMM8_RM32         = 0xC1
  val ROL_IMM8_RM32_SEC     = 0
  val ROR_IMM8_RM32         = 0xC1
  val ROR_IMM8_RM32_SEC     = 1
  val RCL_IMM8_RM32         = 0xC1
  val RCL_IMM8_RM32_SEC     = 2
  val RCR_IMM8_RM32         = 0xC1
  val RCR_IMM8_RM32_SEC     = 3

  /* D1 Prefix */
  val SAR_1_RM32            = 0xD1
  val SAR_1_RM32_SEC        = 7
  val SHL_1_RM32            = 0xD1
  val SHL_1_RM32_SEC        = 4
  val SHR_1_RM32            = 0xD1
  val SHR_1_RM32_SEC        = 5

  val ROL_1_RM32            = 0xD1
  val ROL_1_RM32_SEC        = 0
  val ROR_1_RM32            = 0xD1
  val ROR_1_RM32_SEC        = 1
  val RCL_1_RM32            = 0xD1
  val RCL_1_RM32_SEC        = 2
  val RCR_1_RM32            = 0xD1
  val RCR_1_RM32_SEC        = 3

  /* D3 Prefix */
  val SHL_CL_RM32           = 0xD3
  val SHL_CL_RM32_SEC       = 4
  val SHR_CL_RM32           = 0xD3
  val SHR_CL_RM32_SEC       = 5
  val SAR_CL_RM32           = 0xD3
  val SAR_CL_RM32_SEC       = 7

  val ROL_CL_RM32           = 0xD3
  val ROL_CL_RM32_SEC       = 0
  val ROR_CL_RM32           = 0xD3
  val ROR_CL_RM32_SEC       = 1
  val RCL_CL_RM32           = 0xD3
  val RCL_CL_RM32_SEC       = 2
  val RCR_CL_RM32           = 0xD3
  val RCR_CL_RM32_SEC       = 3

  /* FF Prefix */
  val CALL_RM32             = 0xFF
  val CALL_RM32_SEC         = 2
  val PUSH_RM32             = 0xFF
  val PUSH_RM32_SEC         = 6

  /* 8F Prefix */
  val POP_RM32              = 0x8F
  val POP_RM32_SEC          = 0

  /* C6 Prefix */
  val MOV_IMM8_RM8          = 0xC6
  val MOV_IMM8_RM8_SEC      = 0

  /* C7 Prefix */
  val MOV_IMM32_RM32        = 0xC7
  val MOV_IMM32_RM32_SEC    = 0

  /* 83 Prefix */
  val SUB_IMM8_RM32         = 0x83
  val SUB_IMM8_RM32_SEC     = 5
  val OR_IMM8_RM32          = 0x83
  val OR_IMM8_RM32_SEC      = 1

  // Flag control
  val CLC                   = 0xF8
  val STC                   = 0xF9
  val CMC                   = 0xF5
  val LAHF                  = 0x9F
  val SAHF                  = 0x9E

  val CPUID                 = 0x0F
  val CPUID_SEC             = 0xA2
  val RDTSC                 = 0x0F
  val RDTSC_SEC             = 0x31
  val HLT                   = 0xF4
  val LOCK_PREFIX           = 0xF0
  val XLAT                  = 0xD7

  val CLI                   = 0xFA
  val STI                   = 0xFB

  val MOD_IND_ADDR_NO_DISP = 0b00
  val MOD_IND_ADDR_8_DISP = 0b01
  val MOD_IND_ADDR_32_DISP = 0b10
  val MOD_REG_DIRECT = 0b11

  //RM
  val RM_SIB_BYTE = 0b100
  val RM_ABSOLUTE_ADDRESS = 0b101

  /* SIB */
  val SIB_INDEX_DISABLED = 0b100
  val SIB_BASE_DISABLED = 0b101
}

/**
   * Instruction: <MNEMONIC> (<size/mode if relevant>)
   * Opcode: 0x<OPCODE> [ <secondary opcode / ModRM bits if applicable> ]
   * Operation:
   *   - <High-level description of what the instruction does>
   * Notes:
   *   - <Any quirks, exceptions, undefined behavior, or special cases>
   */

//tail -n +8 disasm.txt | awk '{for(i=1;i<=NF;i++){if($i ~ /^[a-z]+[a-z0-9]*$/){print $i; break}}}' | sort -u
//^^fetches the instruction list of the disassembly