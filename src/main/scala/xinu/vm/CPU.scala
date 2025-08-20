package xinu.vm

import xinu.vm.Constants._
import exceptions.EmulatorPanic
import xinu.vm.Opcodes._

class CPU (memory: Memory) {

  /** Types **/
  private type Flag                =  Int
  private type InstructionHandler  =  () => Unit


  /** Registers **/
  private val registers = Array.fill(8)(0)

  private val EAX = 0
  private val ECX = 1
  private val EDX = 2
  private val EBX = 3
  private val ESP = 4
  private val EBP = 5
  private val ESI = 6
  private val EDI = 7


  /** Instruction Pointer **/
  private var eip = 0


  /** Flags **/
  private var flags           =  0b0000000000000000

  private val CARRY_FLAG      =  0b0000000000000001
  private val PARITY_FLAG     =  0b0000000000000100
  private val ADJUST_FLAG     =  0b0000000000010000
  private val ZERO_FLAG       =  0b0000000001000000
  private val SIGN_FLAG       =  0b0000000010000000
  private val TRAP_FLAG       =  0b0000000100000000
  private val INTERRUPT_FLAG  =  0b0000001000000000
  private val DIRECTION_FLAG  =  0b0000010000000000
  private val OVERFLOW_FLAG   =  0b0000100000000000


  /** Handler Array **/
  private val handlers: Array[InstructionHandler] = Array.fill(256)(unimplementedOpcode)


  /** Method Templates **/

  private def next[T](read: Int => T, size: Int): T = {
    val value = read(eip)
    eip += size
    value
  }

  private def setFlag(flag: Int, value: Boolean): Unit = {
    if (value) { flags |= flag }
    else { flags &= ~flag }
  }

  private def getFlag(flag: Int): Boolean = {
    flag & flags == flag
  }


  /** Read Operations **/
  private def nextByte():  Int  =  next(memory.readByte, 1)
  private def nextInt():   Int  =  next(memory.readInt, 4)

  def step(): Unit = {
    val opcode = nextByte()
    handlers(opcode)()
  }


  /** Flag Setters **/
  private def setCarryFlag     (value: Boolean):  Unit  =  setFlag(CARRY_FLAG, value)
  private def setParityFlag    (value: Boolean):  Unit  =  setFlag(PARITY_FLAG, value)
  private def setAdjustFlag    (value: Boolean):  Unit  =  setFlag(ADJUST_FLAG, value)
  private def setZeroFlag      (value: Boolean):  Unit  =  setFlag(ZERO_FLAG, value)
  private def setSignFlag      (value: Boolean):  Unit  =  setFlag(SIGN_FLAG, value)
  private def setTrapFlag      (value: Boolean):  Unit  =  setFlag(TRAP_FLAG, value)
  private def setInterruptFlag (value: Boolean):  Unit  =  setFlag(INTERRUPT_FLAG, value)
  private def setDirectionFlag (value: Boolean):  Unit  =  setFlag(DIRECTION_FLAG, value)
  private def setOverflowFlag  (value: Boolean):  Unit  =  setFlag(OVERFLOW_FLAG, value)


  /** Flag Getters * */
  private def getCarryFlag:     Boolean  =  getFlag(CARRY_FLAG)
  private def getParityFlag:    Boolean  =  getFlag(PARITY_FLAG)
  private def getAdjustFlag:    Boolean  =  getFlag(ADJUST_FLAG)
  private def getZeroFlag:      Boolean  =  getFlag(ZERO_FLAG)
  private def getSignFlag:      Boolean  =  getFlag(SIGN_FLAG)
  private def getTrapFlag:      Boolean  =  getFlag(TRAP_FLAG)
  private def getInterruptFlag: Boolean  =  getFlag(INTERRUPT_FLAG)
  private def getDirectionFlag: Boolean  =  getFlag(DIRECTION_FLAG)
  private def getOverflowFlag:  Boolean  =  getFlag(OVERFLOW_FLAG)


  def initHandlers(): Unit = {

    handlers(MOV_RM32_R32)    =  handle_mov_rm32_r32
    handlers(MOV_R32_RM32)    =  handle_mov_r32_rm32

    handlers(LEA_M_R32)       =  handle_lea_m_r32

    handlers(ADD_IMM32_EAX)   =  handle_add_imm32_eax
    handlers(ADD_R32_RM32)    =  handle_add_r32_rm32
    handlers(ADD_RM32_R32)    =  handle_add_rm32_r32

    handlers(SUB_IMM32_EAX)   =  handle_sub_imm32_eax
    handlers(SUB_R32_RM32)    =  handle_sub_r32_rm32
    handlers(SUB_RM32_R32)    =  handle_sub_rm32_r32

    handlers(JMP_REL8)        =  handle_jmp_rel8
    handlers(JMP_REL32)       =  handle_jmp_rel32

    handlers(IMUL_RM32_IMM)   =  handle_imul_rm32_imm

    handlers(AND_R32_RM32)    =  handle_and_r32_rm32
    handlers(AND_RM32_R32)    =  handle_and_rm32_r32

    handlers(OR_R32_RM32)     =  handle_or_r32_rm32
    handlers(OR_RM32_R32)     =  handle_or_rm32_r32

    handlers(XOR_R32_RM32)    =  handle_xor_r32_rm32
    handlers(XOR_RM32_R32)    =  handle_xor_rm32_r32

    handlers(JMP_REL8)        =  handle_jmp_rel8
    handlers(JMP_REL32)       =  handle_jmp_rel32
    handlers(JE_REL8)         =  handle_je_rel8
    handlers(JNE_REL8)        =  handle_jne_rel8
    handlers(JG_REL8)         =  handle_jg_rel8
    handlers(JL_REL8)         =  handle_jl_rel8
    handlers(JGE_REL8)        =  handle_jge_rel8
    handlers(JLE_REL8)        =  handle_jle_rel8

    handlers(CALL_REL32)      =  handle_call_rel32
    handlers(RET)             =  handle_ret

    handlers(PUSH_IMM8)       =  handle_push_imm8
    handlers(PUSH_IMM32)      =  handle_push_imm32

    handlers(CMP_R32_RM32)    =  handle_cmp_r32_rm32
    handlers(CMP_RM32_R32)    =  handle_cmp_rm32_r32
    handlers(CMP_IMM32_EAX)   =  handle_cmp_imm32_eax

    handlers(XCHG_R32_RM32)   =  handle_xchg_r32_rm32

    handlers(NOP)             =  handle_nop

    /* Groups with secondary opcodes */
    handlers(0x0F)            =  handle_opcode_0F
    handlers(0x81)            =  handle_opcode_81
    handlers(0xF7)            =  handle_opcode_F7
    handlers(0xC1)            =  handle_opcode_C1
    handlers(0xD3)            =  handle_opcode_D3
    handlers(0xFF)            =  handle_opcode_FF
    handlers(0x8F)            =  handle_opcode_8F
    handlers(0xC6)            =  handle_opcode_C6
    handlers(0xC7)            =  handle_opcode_C7
    handlers(0xF2)            =  handle_opcode_F2
    handlers(0xF3)            =  handle_opcode_F3

    for (i <- 0 to 7) {
      handlers(INC_R32_BASE + i) = () => handle_inc_r32(i)
      handlers(DEC_R32_BASE + i) = () => handle_dec_r32(i)
      handlers(PUSH_R32_BASE + i) = () => handle_push_r32(i)
      handlers(POP_R32_BASE + i) = () => handle_pop_r32(i)
      handlers(XCHG_EAX_BASE + i) = () => handle_xchg_eax(i)
      handlers(MOV_IMM32_REG_BASE + i) = () => handle_mov_imm32_reg(i)
    }
  }

  private def unimplementedOpcode(): Unit = {
    val op = memory.readByte(eip) & 0xFF
    throw new NotImplementedError(f"Unimplemented opcode: 0x$op%02X at EIP=0x$eip%08X")
  }


  /** -------------------- Handlers -------------------------
   *       Note: The instructions use AT&T syntax
   *       e.g. MOV IMM32 EAX = Move IMM32 to EAX
   */

  private def handle_mov_imm32_reg(index: Int): Unit = {
    val imm = nextInt()
    registers(index) = imm
  }

  private def handle_mov_imm8_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        setReg8(rm, nextByte().toByte)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.writeByte(addr, nextByte().toByte)
    }
  }

  private def handle_mov_imm32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = nextInt()
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.writeInt(addr, nextInt())
    }
  }

  private def handle_mov_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(reg) = registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        registers(reg) = memory.readInt(addr)
    }
  }

  private def handle_mov_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = registers(reg)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.writeInt(addr, registers(reg))
    }
  }

  private def handle_movsx_rm8_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        registers(reg) = registers(rm).toByte.toInt
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentVal = memory.readInt(addr)
        registers(reg) = currentVal.toByte.toInt
    }
  }

  private def handle_movzx_rm8_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        registers(reg) = registers(rm) & 0xFF
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentVal = memory.readInt(addr)
        registers(reg) = currentVal & 0xFF
    }
  }

  private def handle_lea_m_r32(): Unit = {
    val (mod: Int, reg: Int, rm: Int) = parseModRM(nextByte())
    val addr = computeEffectiveAddress(mod, rm)
    registers(reg) = addr
  }

  private def handle_add_imm32_eax(): Unit = {
    val imm = nextInt()
    registers(EAX) = addWithFlags(registers(EAX), imm)
  }

  private def handle_add_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = addWithFlags(registers(reg), registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, addWithFlags(currentValue, registers(reg)))
    }
  }

  private def handle_add_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(reg) = addWithFlags(registers(reg), registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        registers(reg) = addWithFlags(memory.readInt(addr), registers(reg))
    }
  }

  private def handle_add_imm32_rm32(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = addWithFlags(nextInt(), registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        val toAdd = nextInt()
        memory.writeInt(addr, addWithFlags(toAdd, currentValue))
    }
  }

  private def handle_sub_imm32_eax(): Unit = {
    val imm = nextInt()
    registers(EAX) = subtractWithFlags(registers(EAX), imm)
  }

  private def handle_sub_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = subtractWithFlags(registers(rm), registers(reg))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, subtractWithFlags(currentValue, registers(reg)))
    }
  }

  private def handle_sub_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(reg) = subtractWithFlags(registers(reg), registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val toSubtract = memory.readInt(addr)
        registers(reg) = subtractWithFlags(registers(reg), toSubtract)
    }
  }

  private def handle_sub_imm32_rm32(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = subtractWithFlags(registers(rm), nextInt())
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        val toSubtract = nextInt()
        memory.writeInt(addr, subtractWithFlags(currentValue, toSubtract))
    }
  }

  private def handle_dec_r32(index: Int): Unit = {
    registers(index) -= 1
  }

  private def handle_inc_r32(index: Int): Unit = {
    registers(index) += 1
  }

  private def handle_neg_rm32(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT => registers(rm) = -registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, -currentValue)
    }
  }


  /**
   * Instruction: <MUL> (Single operand, Unsigned)
   * Opcode: 0x<F7 /4>
   * Operation:
   *   - The value stored in the source operand (register or memory location) multiplies
   *     the value stored in EAX and stores the product in EDX:EAX
   * Notes:
   *   - Unsigned multiplication
   *   - This takes in one operand: the source
   *   - EAX is an implied second operand
   *   - The 32 lower order bits get stored in EAX, the higher 32 get stored in EDX
   */

  private def handle_mul_rm32(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())

    val op1 = registers(EAX)
    val op2 = mod match {
      case MOD_REG_DIRECT =>
        registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.readInt(addr)
    }

    val product = unsignedMultiplyWithFlags(op1, op2)
    registers(EAX) = product & 0xFFFFFFFF // 32 lowest order bits
    registers(EDX) = product >>> 32 // 32 highest order bits
  }


  /**
   * Instruction: <IMUL> (Single Operand, Signed)
   * Opcode: 0x<F7 /5>
   * Operation:
   *   - The value stored in the source operand (register or memory location) multiplies
   *     the value stored in EAX and stores the product in EDX:EAX
   * Notes:
   *   - Signed multiplication
   *   - This takes in one operand: the source. EAX is an implied second operand
   *   - The 32 lower order bits get stored in EAX, the higher 32 get stored in EDX
   */

  private def handle_imul_rm32_single(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())

    val op1 = registers(EAX)
    val op2 = mod match {
      case MOD_REG_DIRECT =>
        registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.readInt(addr)
    }

    val product = signedMultiplyWithFlags(op1, op2)
    registers(EAX) = product & 0xFFFFFFFF // 32 lowest order bits
    registers(EDX) = product >> 32 // 32 highest order bits
  }


  /**
   * Instruction: <IMUL> (ModRM SRC, Signed)
   * Opcode: 0x<0F /AF> [ /r ]
   * Operation:
   *   - <Two-Operands>: Take product of source and destination and store in destination
   *   - <Three-Operands>: Take product of source operands and store them in destination
   * Notes:
   *   - This instruction can take either 2 or 3 operands.
   *   - <Two-Operands>
   *     - In this case, know that reg will equal rm
   *     - Op1 reads in the source operand and op2 reads in the destination
   *   - <Three-Operands>
   *     - Op1 is the first source, op2 is the second source, and reg is the destination
   */

  private def handle_imul_rm32_rm(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    val op1 = mod match {
      case MOD_REG_DIRECT => registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.readInt(addr)
    }
    val op2 = registers(reg)

    registers(reg) = signedMultiplyWithFlags(op1, op2).toInt // result gets truncated
  }


  /**
   * Instruction: <IMUL> (Immediate Value SRC, Signed)
   * Opcode: 0x<69> [ /r ]
   * Operation:
   *   - <Two-Operands>: Take product of source and destination and store in destination
   *   - <Three-Operands>: Take product of source operands and store them in destination
   * Notes:
   *   - This instruction can take either 2 or 3 operands
   *     - <Two-Operands>
   *       - In this case, know that reg will equal rm
   *       - Op1 reads in the destination operand and op2 reads in the immediate value
   *     - <Three-Operands>
   *       - Op1 is the first source and op2 is the second source.
   */

  private def handle_imul_rm32_imm(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val op1 = mod match {
      case MOD_REG_DIRECT => registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.readInt(addr)
    }
    val op2 = memory.readInt(eip)

    registers(reg) = signedMultiplyWithFlags(op1, op2).toInt // result gets truncated
  }


  /**
   * Instruction: <DIV> (Unsigned)
   * Opcode: 0x<F7 /6>
   * Operands:
   *   - SRC: Memory location or register
   *   - DST: EDX:EAX
   * Operation:
   *   - The value in EDX:EAX gets divided by the source operand
   *   - The quotient is stored in EAX and the remainder in EDX
   * Notes:
   *   - Exceptions thrown if quotient or remainder cannot be fit into 32 bits
   */

  private def handle_div_rm32(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())

    val lowBits = registers(EAX).toLong & UINT32_MAX_LONG
    val highBits = registers(EDX).toLong & UINT32_MAX_LONG
    val dividend = (highBits << 32) | lowBits

    val divisor = mod match {
      case MOD_REG_DIRECT =>
        registers(rm).toLong & UINT32_MAX_LONG
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.readInt(addr).toLong & UINT32_MAX_LONG
    }
    if (divisor == 0) throw new ArithmeticException("Divide error: Divide by zero")

    val quotient = dividend / divisor
    val remainder = dividend % divisor

    if (quotient > Int.MaxValue) throw new ArithmeticException("Divide error: quotient overflow")
    if (remainder > Int.MaxValue) throw new ArithmeticException("Divide error: remainder overflow")

    registers(EAX) = quotient.toInt
    registers(EDX) = remainder.toInt
  }


  /**
   * Instruction: <IDIV> (Signed)
   * Opcode: 0x<F7 /7>
   * Operands:
   *   - SRC: Memory location or register
   *   - DST: EDX:EAX
   * Operation:
   *   - The value in EDX:EAX gets divided by the source operand
   *   - The quotient is stored in EAX and the remainder in EDX
   * Notes:
   *   - Exceptions thrown if quotient cannot be fit into 32 bits or divide by zero
   */

  private def handle_idiv_rm32(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())

    val lowBits = registers(EAX).toLong & UINT32_MAX_LONG
    val highBits = registers(EDX).toLong
    val dividend = (highBits << 32) | lowBits

    val divisor = mod match {
      case MOD_REG_DIRECT =>
        registers(rm).toLong
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.readInt(addr).toLong
    }

    if (divisor == 0) throw new ArithmeticException("Divide error: division by zero")

    val quotient = dividend / divisor
    val remainder = dividend % divisor

    if (quotient < Int.MinValue || quotient > Int.MaxValue) throw new ArithmeticException("Divide error: quotient overflow")

    registers(EAX) = quotient.toInt
    registers(EDX) = remainder.toInt
  }


  /**
   * Instruction: <AND> (R32, RM32)
   * Opcode: 0x<21> [ /r ]
   * Operation:
   *   - Does AND operation on R32 and RM32 and stores result in RM32
   */

  private def handle_and_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val op1 = registers(reg)

    mod match {
      case MOD_REG_DIRECT =>
        val op2 = registers(rm)
        registers(rm) = andWithFlags(op1, op2)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val op2 = memory.readInt(addr)
        memory.writeInt(addr, andWithFlags(op1, op2))
    }
  }


  /**
   * Instruction: <AND> (RM32, R32)
   * Opcode: 0x<23> [ /r ]
   * Operation:
   *   - Does AND operation on R32 and RM32 and stores result in R32
   */

  private def handle_and_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val op1 = registers(reg)

    mod match {
      case MOD_REG_DIRECT =>
        registers(reg) = andWithFlags(op1, registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        registers(reg) = andWithFlags(op1, currentValue)
    }
  }


  /**
   * Instruction: <AND> (IMM32, RM32)
   * Opcode: 0x<81> [ /4 id ]
   * Operation:
   *   - Does AND operation on IMM32 & RM32 and stores result in RM32
   */

  private def handle_and_imm_rm32(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val imm = nextInt()
        registers(rm) = andWithFlags(imm, registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)

        val imm = nextInt()
        memory.writeInt(addr, andWithFlags(imm, currentValue))
    }
  }


  /**
   * Instruction: <OR> (R32, RM32)
   * Opcode: 0x<09> [ /r ]
   * Operation:
   *   - Performs OR operation on R32 & RM32 and stores result in RM32
   */

  private def handle_or_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val op1 = registers(reg)

    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = orWithFlags(op1, registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, orWithFlags(op1, currentValue))
    }
  }


  /**
   * Instruction: <OR> (R32, RM32)
   * Opcode: 0x<0B> [ /r ]
   * Operation:
   *   - Performs OR operation on RM32 & R32 and stores result in R32
   */

  private def handle_or_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val op1 = registers(reg)

    mod match {
      case MOD_REG_DIRECT =>
        registers(reg) = orWithFlags(op1, registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        registers(reg) = orWithFlags(op1, currentValue)
    }
  }


  /**
   * Instruction: <OR> (IMM, RM32)
   * Opcode: 0x<0B> [ /1 id ]
   * Operation:
   *   - Performs OR operation on IMM32 & RM32 and stores result in RM32
   */

  private def handle_or_imm32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val imm = nextInt()
        registers(rm) = orWithFlags(imm, registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        val imm = nextByte()
        memory.writeInt(addr, orWithFlags(imm, currentValue))
    }
  }


  /**
   * Instruction: <XOR> (R32, RM32)
   * Opcode: 0x<31> [ /r ]
   * Operation:
   *   - Performs XOR operation on R32 & RM32 and stores result in RM32
   */
  private def handle_xor_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val op1 = registers(reg)

    mod match {
      case MOD_REG_DIRECT =>
        val op2 = registers(rm)
        registers(rm) = xorWithFlags(op1, op2)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val op2 = memory.readInt(addr)
        registers(rm) = memory.writeInt(addr, xorWithFlags(op1, op2))
    }
  }


  /**
   * Instruction: <XOR> (RM32, R32)
   * Opcode: 0x<33> [ /r ]
   * Operation:
   *   - Performs XOR operation on RM32 & R32 and stores result in R32
   */

  private def handle_xor_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val op2 = registers(reg)

    mod match {
      case MOD_REG_DIRECT =>
        val op1 = registers(rm)
        registers(reg) = xorWithFlags(op1, op2)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val op1 = memory.readInt(addr)
        registers(reg) = xorWithFlags(op1, op2)
    }
  }


  /**
   * Instruction: <XOR> (IMM32, RM32)
   * Opcode: 0x<81> [ /6 id ]
   * Operation:
   *   - Performs XOR operation on IMM32 & RM32 and stores result in RM32
   */

  private def handle_xor_imm32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val imm = nextInt()
        registers(rm) = xorWithFlags(imm, registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        val imm = nextInt()
        memory.writeInt(addr, xorWithFlags(imm, currentValue))
    }
  }


  /**
   * Instruction: <SHL> (IMM8, RM32)
   * Opcode: 0x<C1> [ < /4 ib > ]
   * Operation:
   *   - Shift bits in RM32 left by value stored in IMM8
   */

  private def handle_shl_imm8_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val shiftDistance = nextInt()
        val currentValue = registers(rm)
        registers(rm) = shlWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val shiftDistance = nextInt()
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, shlWithFlags(currentValue, shiftDistance))
    }
  }

  private def handle_shl_1_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val shiftDistance = 1
        val currentValue = registers(rm)
        registers(rm) = shlWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val shiftDistance = 1
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, shlWithFlags(currentValue, shiftDistance))
    }
  }


  /**
   * Instruction: <SHL> (IMM8, RM32)
   * Opcode: 0x<D3> [ < /4 > ]
   * Operation:
   *   - Shift bits in RM32 left by value stored in CL register
   */

  private def handle_shl_cl_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val shiftDistance = getCL.toInt

    mod match {
      case MOD_REG_DIRECT =>
        val currentValue = registers(rm)
        registers(rm) = shlWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, shlWithFlags(currentValue, shiftDistance))
    }
  }


  /**
   * Instruction: <SHR> (IMM8, RM32)
   * Opcode: 0x<C1> [ < /7 > ]
   * Operation:
   *   - Shift bits in RM32 right by IMM8 (logical)
   */

  private def handle_shr_imm8_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val shiftDistance = nextInt()
        val currentValue = registers(rm)
        registers(rm) = shrWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val shiftDistance = nextInt()
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, shrWithFlags(currentValue, shiftDistance))
    }
  }

  private def handle_shr_1_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val shiftDistance = 1
        val currentValue = registers(rm)
        registers(rm) = shrWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val shiftDistance = 1
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, shrWithFlags(currentValue, shiftDistance))
    }
  }


  /**
   * Instruction: <SHR> (CL, RM32)
   * Opcode: 0x<D3> [ < /7 > ]
   * Operation:
   *   - Shift bits in RM32 right by CL (logical)
   */

  private def handle_shr_cl_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val shiftDistance = getCL.toInt

    mod match {
      case MOD_REG_DIRECT =>
        val currentValue = registers(rm)
        registers(rm) = shrWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, shrWithFlags(currentValue, shiftDistance))
    }
  }


  /**
   * Instruction: <SHR> (IMM8, RM32)
   * Opcode: 0x<C1> [ < /7 > ]
   * Operation:
   *   - Shift bits in RM32 right by IMM8 (arithmetic)
   */

  private def handle_sar_imm8_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val shiftDistance = nextInt()
        val currentValue = registers(rm)
        registers(rm) = shrWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val shiftDistance = nextInt()
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, sarWithFlags(currentValue, shiftDistance))
    }
  }

  private def handle_sar_1_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val shiftDistance = 1
        val currentValue = registers(rm)
        registers(rm) = shrWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val shiftDistance = 1
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, sarWithFlags(currentValue, shiftDistance))
    }
  }



  /**
   * Instruction: <SHR> (CL, RM32)
   * Opcode: 0x<D3> [ < /7 > ]
   * Operation:
   *   - Shift bits in RM32 right by CL (arithmetic)
   */

  private def handle_sar_cl_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val shiftDistance = getCL.toInt

    mod match {
      case MOD_REG_DIRECT =>
        val currentValue = registers(rm)
        registers(rm) = shrWithFlags(currentValue, shiftDistance)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, sarWithFlags(currentValue, shiftDistance))
    }
  }

  private def handle_push_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    registers(ESP) -= 4

    mod match {
      case MOD_REG_DIRECT =>
        memory.writeInt(registers(ESP), registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val toWrite = memory.readInt(addr)
        memory.writeInt(registers(ESP), toWrite)
    }
  }

  private def handle_push_r32(index: Int): Unit = {
    if (memory.overflowsStack(4, ESP)) throw EmulatorPanic(s"Stack overflow at ESP=0x${ESP.toHexString}")
    registers(ESP) -= 4
    memory.writeInt(registers(ESP), registers(index))
  }

  private def handle_push_imm8(): Unit = {
    val toWrite = nextByte()
    push(toWrite)
  }

  private def handle_push_imm32(): Unit = {
    val toWrite = nextInt()
    push(toWrite)
  }

  private def handle_pop_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = pop()
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val toWrite = pop()
        memory.writeInt(addr, toWrite)
    }
  }

  private def handle_pop_r32(index: Int): Unit = {
    registers(index) = pop()
  }

  private def handle_jmp_rel32(): Unit = {
    val rel = nextInt()
    eip += rel
  }

  private def handle_jmp_rel8(): Unit = {
    val rel = nextByte()
    eip += rel
  }

  private def handle_je_rel32(): Unit = {
    if (getZeroFlag) eip += nextInt()
  }

  private def handle_je_rel8(): Unit = {
    if (getZeroFlag) eip += nextByte()
  }

  private def handle_jne_rel8(): Unit = {
    if (!getZeroFlag) eip += nextByte()
  }

  private def handle_jne_rel32(): Unit = {
    if (!getZeroFlag) eip += nextInt()
  }

  private def handle_jg_rel8(): Unit = {
    if (!getZeroFlag && (getSignFlag == getOverflowFlag)) {
      eip += nextByte()
    }
  }

  private def handle_jg_rel32(): Unit = {
    if (!getZeroFlag && (getSignFlag == getOverflowFlag)) {
      eip += nextInt()
    }
  }

  private def handle_jl_rel8(): Unit = {
    if (getSignFlag != getOverflowFlag) eip += nextByte()
  }

  private def handle_jl_rel32(): Unit = {
    if (getSignFlag != getOverflowFlag) eip += nextInt()
  }

  private def handle_jge_rel8(): Unit = {
    if (getSignFlag == getOverflowFlag) eip += nextByte()
  }

  private def handle_jge_rel32(): Unit = {
    if (getSignFlag == getOverflowFlag) eip += nextInt()
  }

  private def handle_jle_rel8(): Unit = {
    if (getZeroFlag || (getSignFlag != getOverflowFlag)) eip += nextByte()
  }

  private def handle_jle_rel32(): Unit = {
    if (getZeroFlag || (getSignFlag != getOverflowFlag)) eip += nextInt()
  }

  private def handle_call_rel32(): Unit = {
    val newEip = eip + nextInt()
    push32(eip)
    eip = newEip
  }

  private def handle_call_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val newEip = registers(rm)
        push32(eip)
        eip = newEip
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val newEip = memory.readInt(addr)
        push32(eip)
        eip = newEip
    }
  }

  private def handle_ret(): Unit = {
    eip = pop()
  }

  private def handle_cmp_imm32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        subtractWithFlags(registers(rm), nextInt())
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        subtractWithFlags(memory.readInt(addr), nextInt())
    }
  }

  private def handle_cmp_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        subtractWithFlags(registers(rm), registers(reg))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        subtractWithFlags(memory.readInt(addr), registers(reg))
    }
  }

  private def handle_cmp_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        subtractWithFlags(registers(reg), registers(rm))
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        subtractWithFlags(registers(reg), memory.readInt(addr))
    }
  }

  private def handle_cmp_imm32_eax(): Unit = {
    subtractWithFlags(registers(EAX), nextInt())
  }

  private def handle_seto_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (getOverflowFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_setno_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (!getOverflowFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_setnae_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (getCarryFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_setnb_rm8(): Unit =  {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (!getCarryFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_sete_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (getZeroFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_setne_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (!getZeroFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_setna_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (getCarryFlag || getZeroFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_setnbe_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (!getCarryFlag && !getZeroFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_sets_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (getSignFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_setns_rm8(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    val value = if (!getSignFlag) 1 else 0
    setByteRM(mod, rm, value.toByte)
  }

  private def handle_xchg_eax(index: Int): Unit = {
    val temp = registers(EAX)
    registers(EAX) = registers(index)
    registers(index) = temp
  }

  private def handle_xchg_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    mod match {
      case MOD_REG_DIRECT =>
        val temp = registers(reg)
        registers(reg) = registers(rm)
        registers(rm) = temp
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val temp = registers(reg)
        registers(reg) = memory.readInt(addr)
        memory.writeInt(addr, temp)
    }
  }

  private def handle_nop(): Unit = {
    // do nothing
  }

  private def handle_movsb(): Unit = {
    val toWrite = memory.readByte(registers(ESI)).toByte
    memory.writeByte(registers(EDI), toWrite)
  }

  private def handle_rep_movsb(): Unit = {
    val direction = if (getDirectionFlag) -1 else 1

    while (registers(ECX) > 0) {
      val toWrite = memory.readByte(registers(ESI))
      memory.writeByte(registers(EDI), toWrite.toByte)
      registers(ESI) += direction
      registers(EDI) += direction
      registers(ECX) -= 1
    }
  }

  private def handle_movsd(): Unit = {
    val toWrite = memory.readInt(registers(ESI))
    memory.writeInt(registers(EDI), toWrite)
  }

  private def handle_rep_movsd(): Unit = {
    val direction = if (getDirectionFlag) -4 else 4

    while (registers(ECX) > 0) {
      val toWrite = memory.readInt(registers(ESI))
      memory.writeInt(registers(EDI), toWrite)
      registers(ESI) += direction
      registers(EDI) += direction
      registers(ECX) -= 1
    }
  }

  private def handle_stosb(): Unit = {
    memory.writeByte(registers(EDI), getAL)
  }

  private def handle_rep_stosb(): Unit = {
    val direction = if (getDirectionFlag) -1 else 1

    var count = registers(ECX)
    while (count > 0) {
      val toWrite = getAL
      memory.writeByte(registers(EDI), toWrite)
      registers(EDI) += direction
      count -= 1
    }
  }

  private def handle_stosd(): Unit = {
    memory.writeInt(registers(EDI), registers(EAX))
  }

  private def handle_rep_stosd(): Unit = {
    val direction = if (getDirectionFlag) -4 else 4

    var count = registers(ECX)
    while (count > 0) {
      memory.writeInt(registers(EDI), registers(EAX))
      registers(EDI) += direction
      count -= 1
    }
  }


  // NOTE: LODS can have REP prefix technically, but it never seems to be used
  // as it doesn't make sense to use it
  private def handle_lodsb(): Unit = {
    setAL(memory.readByte(registers(ESI)).toByte)
  }

  private def handle_lodsd(): Unit = {
    registers(EAX) = memory.readInt(registers(ESI))
  }

  private def handle_scasb(): Unit = {
    val direction = if (getDirectionFlag) -1 else 1
    subtractWithFlags(getAL.toInt, memory.readByte(registers(EDI)))
    registers(EDI) += direction
  }

  private def handle_repe_scasb(): Unit = {
    val direction = if (getDirectionFlag) -1 else 1
    while (registers(ECX) > 0 && getZeroFlag) {
      subtractWithFlags(getAL.toInt, memory.readByte(registers(EDI)))
      registers(EDI) += direction
      registers(ECX) -= 1
    }
  }

  private def handle_repne_scasb(): Unit = {
    val direction = if (getDirectionFlag) -1 else 1
    while (registers(ECX) > 0 && !getZeroFlag) {
      subtractWithFlags(getAL.toInt, memory.readByte(registers(EDI)))
      registers(EDI) += direction
      registers(ECX) -= 1
    }
  }

  private def handle_scasd(): Unit = {
    val direction = if (getDirectionFlag) -4 else 4
    subtractWithFlags(registers(EAX), memory.readByte(registers(EDI)))
    registers(EDI) += direction
  }

  private def handle_repe_scasd(): Unit = {
    val direction = if (getDirectionFlag) -4 else 4
    while (registers(ECX) > 0 && getZeroFlag) {
      subtractWithFlags(registers(EAX), memory.readInt(registers(EDI)))
      registers(EDI) += direction
      registers(ECX) -= 1
    }
  }

  private def handle_repne_scasd(): Unit = {
    val direction = if (getDirectionFlag) -4 else 4
    while (registers(ECX) > 0 && !getZeroFlag) {
      subtractWithFlags(registers(EAX), memory.readInt(registers(EDI)))
      registers(EDI) += direction
      registers(ECX) -= 1
    }
  }

  private def handle_cmpsb(): Unit = {
    subtractWithFlags(memory.readByte(registers(ESI)), memory.readByte(registers(EDI)))
  }

  private def handle_repe_cmpsb(): Unit = {
    val direction = if (getDirectionFlag) -1 else 1

    while (registers(EDX) > 0 && getZeroFlag) {
      subtractWithFlags(memory.readByte(registers(ESI)), memory.readByte(registers(EDI)))
      registers(ESI) += direction
      registers(EDI) += direction
      registers(EDX) -= 1
    }
  }

  private def handle_repne_cmpsb(): Unit = {
    val direction = if (getDirectionFlag) -1 else 1

    while (registers(EDX) > 0 && !getZeroFlag) {
      subtractWithFlags(memory.readByte(registers(ESI)), memory.readByte(registers(EDI)))
      registers(ESI) += direction
      registers(EDI) += direction
      registers(EDX) -= 1
    }
  }

  private def handle_cmpsd(): Unit = {
    subtractWithFlags(memory.readInt(registers(ESI)), memory.readInt(registers(EDI)))
  }

  private def handle_repe_cmpsd(): Unit = {
    val direction = if (getDirectionFlag) -4 else 4

    while (registers(EDX) > 0 && getZeroFlag) {
      subtractWithFlags(memory.readInt(registers(ESI)), memory.readInt(registers(EDI)))
      registers(ESI) += direction
      registers(EDI) += direction
      registers(EDX) -= 1
    }
  }

  private def handle_repne_cmpsd(): Unit = {
    val direction = if (getDirectionFlag) -4 else 4

    while (registers(EDX) > 0 && !getZeroFlag) {
      subtractWithFlags(memory.readInt(registers(ESI)), memory.readInt(registers(EDI)))
      registers(ESI) += direction
      registers(EDI) += direction
      registers(EDX) -= 1
    }
  }


  /** Opcode Group Handlers * */

  private def handle_opcode_0F(): Unit = {
    val opcode = nextByte()

    opcode match {
      case IMUL_RM32_RM_SEC => handle_imul_rm32_rm()
      case JE_REL32_SEC => handle_je_rel32()
      case JNE_REL32_SEC => handle_jne_rel32()
      case JG_REL32_SEC => handle_jg_rel32()
      case JL_REL32_SEC => handle_jl_rel32()
      case JGE_REL32_SEC => handle_jge_rel32()
      case JLE_REL32_SEC => handle_jle_rel32()
      case MOVSX_RM8_R32_SEC => handle_movsx_rm8_r32()
      case MOVZX_RM8_R32_SEC => handle_movzx_rm8_r32()
      case SETO_SEC => handle_seto_rm8()
      case SETNO_SEC => handle_setno_rm8()
      case SETNAE_SEC => handle_setnae_rm8()
      case SETNB_SEC => handle_setnb_rm8()
      case SETE_SEC => handle_sete_rm8()
      case SETNE_SEC => handle_setne_rm8()
      case SETNA_SEC => handle_setna_rm8()
      case SETNBE_SEC => handle_setnbe_rm8()
      case SETS_SEC => handle_sets_rm8()
      case SETNS_SEC => handle_setns_rm8()
      case _ =>
        throw new NotImplementedError(f"Unknown 0F opcode: 0F $opcode%02X")
    }
  }

  private def handle_opcode_F7(): Unit = {
    val opcode = nextByte()

    opcode match {
      case NEG_RM32_SEC => handle_neg_rm32()
      case MUL_RM32_SEC => handle_mul_rm32()
      case IMUL_RM32_SEC => handle_mul_rm32()
      case DIV_RM32_SEC => handle_div_rm32()
      case IDIV_RM32_SEC => handle_idiv_rm32()
      case _ =>
        throw new NotImplementedError(f"Unknown F7 opcode: F7 $opcode%02X")
    }
  }

  private def handle_opcode_81(): Unit = {
    val opcode = nextByte()

    opcode match {
      case ADD_IMM32_RM32_SEC => handle_add_imm32_rm32()
      case SUB_IMM32_RM32_SEC => handle_sub_imm32_rm32()
      case AND_IMM32_RM32_SEC => handle_and_imm_rm32()
      case CMP_IMM32_RM32_SEC => handle_cmp_imm32_rm32()
      case OR_IMM32_RM32_SEC  => handle_or_imm32_rm32()
      case XOR_IMM32_RM32_SEC => handle_xor_imm32_rm32()
      case _ =>
        throw new NotImplementedError(f"Unknown 81 opcode: 81 $opcode%02X")
    }
  }

  private def handle_opcode_C1(): Unit = {
    val opcode = nextByte()

    opcode match {
      case SAR_IMM8_RM32_SEC => handle_sar_imm8_rm32()
      case SHL_IMM8_RM32_SEC => handle_shl_imm8_rm32()
      case SHR_IMM8_RM32_SEC => handle_shr_imm8_rm32()
      case _ =>
        throw new NotImplementedError(f"Unknown C1 opcode: C1 $opcode%02X")
    }
  }

  private def handle_opcode_D3(): Unit = {
    val opcode = nextByte()

    opcode match {
      case SAR_CL_RM32_SEC => handle_sar_cl_rm32()
      case SHL_CL_RM32_SEC => handle_shl_cl_rm32()
      case SHR_CL_RM32_SEC => handle_shr_cl_rm32()
      case _ =>
        throw new NotImplementedError(f"Unknown D3 opcode: D3 $opcode%02X")
    }
  }

  private def handle_opcode_FF(): Unit = {
    val opcode = nextByte()

    opcode match {
      case CALL_RM32_SEC => handle_call_rm32()
      case _ => throw new NotImplementedError(f"Unknown FF opcode: FF $opcode%02X")
    }
  }

  private def handle_opcode_8F(): Unit = {
    val opcode = nextByte()

    opcode match {
      case POP_RM32_SEC => handle_pop_rm32()
      case _ => throw new NotImplementedError(f"Unknown 8F opcode: 8F $opcode%02X")
    }
  }

  private def handle_opcode_C6(): Unit = {
    val opcode = nextByte()

    opcode match {
      case MOV_IMM8_RM8_SEC => handle_mov_imm8_rm8()
      case _ => throw new NotImplementedError(f"Unknown C6 opcode: C6 $opcode%02X")
    }
  }

  private def handle_opcode_C7(): Unit = {
    val opcode = nextByte()

    opcode match {
      case MOV_IMM32_RM32_SEC => handle_mov_imm32_rm32()
      case _ => throw new NotImplementedError(f"Unknown C7 opcode: C7 $opcode%02X")
    }
  }

  private def handle_opcode_F2(): Unit = {
    val opcode = nextByte()
    opcode match {
      case CMPSB => handle_repne_cmpsb()
      case CMPSD => handle_repne_cmpsd()
      case SCASB => handle_repne_scasb()
      case SCASD => handle_repne_scasd()
      case _ => throw new NotImplementedError(f"Unknown F2 opcode: F2 $opcode%02X")
    }
  }

  /* The F3 prefix represents the REP and REPE Prefixes */
  private def handle_opcode_F3(): Unit = {
    val opcode = nextByte()
    opcode match {
      case MOVSB => handle_rep_movsb()
      case MOVSD => handle_rep_movsd()
      case STOSB => handle_rep_stosb()
      case STOSD => handle_rep_stosd()
      case CMPSB => handle_repe_cmpsb()
      case CMPSD => handle_repe_cmpsd()
      case SCASB => handle_repe_scasb()
      case SCASD => handle_repe_scasd()
      case _ => throw new NotImplementedError(f"Unknown F3 opcode: F3 $opcode%02X")
    }
  }

  private def handle_opcode_D1(): Unit = {
    val opcode = nextByte()

    opcode match {
      case SHL_1_RM32_SEC => handle_shl_1_rm32()
      case SHR_1_RM32_SEC => handle_shr_1_rm32()
      case SAR_1_RM32_SEC => handle_sar_1_rm32()
      case _ => throw new NotImplementedError(f"Unknown D1 opcode: D1 $opcode%02X")
    }
  }


  /** Performs the operation specified by op and check methods to see which flags
   *  should be set, some of which are specified in the parameters, then sets them.
   *  Used by addWithFlags and subtractWithFlags
   */
  def updateFlagsArithmetic
  (a: Int, b: Int, op: (Int, Int) => Int,
   detectCarry: (Int, Int, Int) => Boolean,
   detectOverflow: (Int, Int, Int) => Boolean): Int =
  {
    val result = op(a,b)

    setCarryFlag(detectCarry(a, b, result))
    setParityFlag(hasEvenParity(result))
    setAdjustFlag(detectAdjust(a, b, result))
    setZeroFlag(isZero(result))
    setSignFlag(isNegative(result))
    setOverflowFlag(detectOverflow(a, b, result))

    result
  }

  /** Performs Add operation and then sets flags **/
  private def addWithFlags(a: Int, b: Int): Int = {
    updateFlagsArithmetic(
      a, b, _ + _,
      (a, b, res) => (a & UINT32_MAX_LONG) + (b & UINT32_MAX_LONG) > UINT32_MAX_LONG, // cf check
      (a, b, res) => ((a ^ res) & (b ^ res) & SIGN_BIT_MASK) != 0 // of check
    )
  }

  /** Performs Subtract operation and then sets flags */
  private def subtractWithFlags(a: Int, b: Int): Int = {
    updateFlagsArithmetic(
      a, b, _ - _,
      (a, b, res) => Integer.compareUnsigned(a, b) < 0, // cf check
      (a, b, res) => ((a ^ res) & (b ^ res) & SIGN_BIT_MASK) != 0 // of check
    )
  }

  /** Performs unsigned multiplication and then sets flags */
  private def unsignedMultiplyWithFlags(a: Int, b: Int): Long = {
    val result = (a.toLong & UINT32_MAX_LONG) * (b.toLong & UINT32_MAX_LONG) // treats values as unsigned integers
    val flagResult = result >>> 32 != 0 // true if value cannot fit inside 32 bits

    setCarryFlag(flagResult)
    setOverflowFlag(flagResult)
    result
  }

  /** Performs signed multiplication and then sets flags */
  private def signedMultiplyWithFlags(a: Int, b: Int): Long = {
    val result = a.toLong * b.toLong
    val flagResult = result >> 32 != 0 // true if value cannot fit inside 32 bits

    setCarryFlag(flagResult)
    setOverflowFlag(flagResult)
    result
  }

  /** Performs AND operation and then sets flags */
  private def andWithFlags(a: Int, b: Int): Int = {
    val result = a & b

    /* These are always false after the operation */
    setCarryFlag(false)
    setOverflowFlag(false)

    setParityFlag(hasEvenParity(result))
    setAdjustFlag(detectAdjust(a, b, result))
    setZeroFlag(isZero(result))
    result

  }

  /** Performs OR operation and then sets flags */
  private def orWithFlags(a: Int, b: Int): Int = {
    val result = a | b

    /* These are always false after the operation */
    setCarryFlag(false)
    setOverflowFlag(false)

    setParityFlag(hasEvenParity(result))
    setAdjustFlag(detectAdjust(a, b, result))
    setZeroFlag(isZero(result))
    result
  }

  /** Performs XOR operation and then sets flags */
  private def xorWithFlags(a: Int, b: Int): Int = {
    val result = a ^ b

    /* These are always false after the operation */
    setCarryFlag(false)
    setOverflowFlag(false)

    setParityFlag(hasEvenParity(result))
    setAdjustFlag(detectAdjust(a, b, result))
    setZeroFlag(isZero(result))
    result
  }


  private def shlWithFlags(a: Int, shiftDistance: Int): Int = {
    val trueDistance = shiftDistance & 0x1F  //ensures shift distance is no larger than 5 bi
    //val cf_mask = 0x10000000 >> (trueDistance - 1)  //mask for last bit shifted out
    val result = a << trueDistance
    val cf_value = ((a >>> (32 - trueDistance)) & 0x1) != 0

    if (trueDistance == 1) {
      val of_value = ((a >> 31) ^ (a >> 30)) != 0
      setOverflowFlag(of_value)
    }

    setParityFlag(hasEvenParity(result))
    setSignFlag(isNegative(result))
    setZeroFlag(isZero(result))
    result
  }

  private def shrWithFlags(a: Int, shiftDistance: Int): Int = {
    val trueDistance = shiftDistance & 0x1F  //ensures shift distance is no larger than 5 bits
    val result = a >>> trueDistance

    if (trueDistance > 0) {
      val cf_value = ((a >>> (trueDistance - 1)) & 0x1) != 0
      setCarryFlag(cf_value)
    }
    if (trueDistance == 1) {
      val of_mask = 0x00000001 //mask for most significant bit after shift
      val of_value = (a & 0x80000000) != 0
      setOverflowFlag(of_value)
    }

    setParityFlag(hasEvenParity(result))
    setSignFlag(isNegative(result))
    setZeroFlag(isZero(result))
    result
  }

  private def sarWithFlags(a: Int, shiftDistance: Int): Int = {
    val trueDistance = shiftDistance & 0x1F //ensures shift distance is no larger than 5 bits
    val result = a >> trueDistance

    if (trueDistance > 0) {
      val cf_value = ((a >>> (trueDistance - 1)) & 0x1) != 0
      setCarryFlag(cf_value)
    }
    if (trueDistance == 1) setOverflowFlag(false)

    setParityFlag(hasEvenParity(result))
    setSignFlag(isNegative(result))
    setZeroFlag(isZero(result))
    result
  }

  /** Computes the effective result of SIB. Does not include displacement. * */
  private def computeSIB(scale: Int, index: Int, base: Int) = {
    val trueScale = 1 << scale //converts scale bits into the true scale
    val result = (base, index) match {
      case (SIB_BASE_DISABLED, SIB_INDEX_DISABLED) => 0
      case (SIB_BASE_DISABLED, _) => registers(index) * trueScale
      case (_, SIB_INDEX_DISABLED) => registers(base)
      case (_, _) => registers(base) + (registers(index) * trueScale)
    }
    result
  }

  /** Reads the displacement, eip must be positioned prior **/
  private def readDisplacement(mod: Int, rm: Int): Int = (mod, rm) match {
    case (MOD_IND_ADDR_NO_DISP, RM_ABSOLUTE_ADDRESS) => nextInt()
    case (MOD_IND_ADDR_8_DISP, _) => nextByte()
    case (MOD_IND_ADDR_32_DISP, _) => nextInt()
    case (_, _) => 0
  }

  /** Computes the memory address given mod and rm */
  private def computeEffectiveAddress(mod: Int, rm: Int): Int = rm match {
    case RM_SIB_BYTE =>
      val (scale, index, base) = parseSIB(nextByte())
      val displacement = readDisplacement(mod, rm)
      val addr = computeSIB(scale, index, base) + displacement
      addr
    case RM_ABSOLUTE_ADDRESS =>
      readDisplacement(mod, rm) // displacement returns address in this case
    case _ => //  register + displacement
      val displacement = readDisplacement(mod, rm)
      registers(rm) + displacement
  }

  /** Extract the mod, reg, and rm bits from the ModRM byte * */
  private def parseModRM(modRM: Int): (Int, Int, Int) = {
    ((modRM >> 6) & 0b11, (modRM >> 3) & 0b111, modRM & 0b111)
  }

  /** Extract the scale, index, and base bits from SIB byte * */
  private def parseSIB(sib: Int): (Int, Int, Int) = {
    ((sib >> 6) & 0b11, (sib >> 3) & 0b111, sib & 0b111)
  }


  /** Used to indicate the unsigned sum is greater than what can fit in the destination. */
  private def addCarry(op1: Int, op2: Int): Boolean = {
    // This removes the sign bit so that the values are treated as unsigned
    (op1.toLong & UINT32_MAX_LONG) + (op2.toLong & UINT32_MAX_LONG) > UINT32_MAX_LONG
  }

  private def hasEvenParity(value: Int): Boolean = (Integer.bitCount(value) & 1) == 0
  private def isZero(value: Int): Boolean = value == 0
  private def isNegative(value: Int): Boolean = value & SIGN_BIT_MASK != 0
  private def zeroExtendInt(i: Int): Long = i & UINT32_MAX
  private def detectAdjust(a: Int, b: Int, result: Int): Boolean = ((a ^ b ^ result) & 0x10) != 0
  private def getCL: Byte = (registers(ECX) & 0xFF).toByte
  private def setCL(value: Byte): Unit = {
    registers(ECX) = (registers(ECX) & 0xFFFFFF00) | (value & 0xFF)
  }
  private def getAL: Byte = getReg8(EAX)
  private def setAL(value: Byte): Unit = setReg8(EAX, value)
  private def getReg8(reg: Int): Byte = (registers(reg) & 0xFF).toByte
  private def setReg8(reg: Int, value: Byte): Unit = {
    registers(reg) = (registers(reg) & 0xFFFFFF00) | (value & 0xFF)
  }

  private def setByteRM(mod: Int, rm: Int, value: Byte): Unit = {
    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = (registers(rm) & 0xFFFFFF00) | (value & 0xFF)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readByte(addr)
        memory.writeByte(addr, value)
    }
  }

  private def push(toPush: Int): Unit = {
    if (memory.overflowsStack(4, ESP)) throw EmulatorPanic(s"Stack overflow at ESP=0x${ESP.toHexString}")
    registers(ESP) -= 4
    memory.writeInt(registers(ESP), toPush)
  }

  private def pop(): Int = {
    if (memory.underflowsStack(4, ESP)) throw EmulatorPanic(s"Stack underflow at ESP=0x${ESP.toHexString}")
    val toReturn = memory.readInt(registers(ESP))
    registers(ESP) += 4
    toReturn
  }

  private def pop_reg(index: Int): Unit = {
    if (memory.underflowsStack(4, ESP)) throw EmulatorPanic(s"Stack underflow at ESP=0x${ESP.toHexString}")
    registers(index) = memory.readInt(registers(ESP))
    registers(ESP) += 4
  }

  private def pop_mem(addr: Int): Unit = {
    if (memory.underflowsStack(4, ESP)) throw EmulatorPanic(s"Stack underflow at ESP=0x${ESP.toHexString}")
    val toWrite = memory.readInt(registers(ESP))
    memory.writeInt(addr, toWrite)
    registers(ESP) += 4
  }
}
