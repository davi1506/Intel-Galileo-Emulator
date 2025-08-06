package xinu.vm

import xinu.vm.Constants._
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

  private def getFlag(flag: Int): Int = {
    flag & flags
  }


  /** Read Operations **/
  private def nextByte():  Byte  =  next(memory.readByte, 1)
  private def nextInt():   Int   =  next(memory.readInt, 4)

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
  private def getCarryFlag:     Int  =  getFlag(CARRY_FLAG)
  private def getParityFlag:    Int  =  getFlag(PARITY_FLAG)
  private def getAdjustFlag:    Int  =  getFlag(ADJUST_FLAG)
  private def getZeroFlag:      Int  =  getFlag(ZERO_FLAG)
  private def getSignFlag:      Int  =  getFlag(SIGN_FLAG)
  private def getTrapFlag:      Int  =  getFlag(TRAP_FLAG)
  private def getInterruptFlag: Int  =  getFlag(INTERRUPT_FLAG)
  private def getDirectionFlag: Int  =  getFlag(DIRECTION_FLAG)
  private def getOverflowFlag:  Int  =  getFlag(OVERFLOW_FLAG)


  def initHandlers(): Unit = {

    handlers(Opcodes.MOV_IMM32_EAX)   =  handle_mov_imm32_eax
    handlers(Opcodes.MOV_IMM32_ECX)   =  handle_mov_imm32_ecx
    handlers(Opcodes.MOV_IMM32_EDX)   =  handle_mov_imm32_edx
    handlers(Opcodes.MOV_IMM32_EBX)   =  handle_mov_imm32_ebx
    handlers(Opcodes.MOV_IMM32_ESP)   =  handle_mov_imm32_esp
    handlers(Opcodes.MOV_IMM32_EBP)   =  handle_mov_imm32_ebp
    handlers(Opcodes.MOV_IMM32_ESI)   =  handle_mov_imm32_esi
    handlers(Opcodes.MOV_IMM32_EDI)   =  handle_mov_imm32_edi

    handlers(Opcodes.MOV_RM32_R32)    =  handle_mov_rm32_r32
    handlers(Opcodes.MOV_R32_RM32)    =  handle_mov_r32_rm32

    handlers(Opcodes.LEA_M_R32)       =  handle_lea_m_r32

    handlers(Opcodes.ADD_IMM32_EAX)   =  handle_add_imm32_eax
    handlers(Opcodes.ADD_R32_RM32)    =  handle_add_r32_rm32
    handlers(Opcodes.ADD_RM32_R32)    =  handle_add_rm32_r32
    handlers(Opcodes.ADD_IMM32_RM32)  =  handle_add_imm32_rm32

    handlers(Opcodes.SUB_IMM32_EAX)   =  handle_sub_imm32_eax
    handlers(Opcodes.SUB_R32_RM32)    =  handle_sub_r32_rm32
    handlers(Opcodes.SUB_RM32_R32)    =  handle_sub_rm32_r32
    handlers(Opcodes.SUB_IMM32_RM32)  =  handle_sub_imm32_rm32

    handlers(Opcodes.JMP_REL8)        =  handleJmpRel8
    handlers(Opcodes.JMP_REL32)       =  handleJmpRel32

    handlers(0x0F)                    =  handle_opcode_0F
    handlers(0xF7)                    =  handle_opcode_F7

    for (i <- 0 to 7) {
      handlers(Opcodes.INC_R32_BASE + i) = () => handle_inc_r32(i)
    }

    for (i <- 0 to 7) {
      handlers(Opcodes.DEC_R32_BASE + i) = () => handle_dec_r32(i)
    }

    for (i <- 0 to 7) {
      handlers(Opcodes.PUSH_R32_BASE + i) = () => handlePushReg(i)
    }

    handlers(0x0F) = handle_opcode_0F
  }

  private def unimplementedOpcode(): Unit = {
    val op = memory.readByte(eip) & 0xFF
    throw new NotImplementedError(f"Unimplemented opcode: 0x$op%02X at EIP=0x$eip%08X")
  }


  /** -------------------- Handlers -------------------------
   *       Note: The instructions use AT&T syntax
   *       e.g. MOV IMM32 EAX = Move IMM32 to EAX
   */

  private def handle_mov_imm32_eax(): Unit = {
    val imm = nextInt()
    registers(EAX) = imm
  }

  private def handle_mov_imm32_ecx(): Unit = {
    val imm = nextInt()
    registers(ECX) = imm
  }

  private def handle_mov_imm32_edx(): Unit = {
    val imm = nextInt()
    registers(EDX) = imm
  }

  private def handle_mov_imm32_ebx(): Unit = {
    val imm = nextInt()
    registers(EBX) = imm
  }

  private def handle_mov_imm32_esp(): Unit = {
    val imm = nextInt()
    registers(ESP) = imm
  }

  private def handle_mov_imm32_ebp(): Unit = {
    val imm = nextInt()
    registers(EBP) = imm
  }

  private def handle_mov_imm32_esi(): Unit = {
    val imm = nextInt()
    registers(ESI) = imm
  }

  private def handle_mov_imm32_edi(): Unit = {
    val imm = nextInt()
    registers(EDI) = imm
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
    registers(EAX) -= imm
  }

  private def handle_sub_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) -= registers(reg)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, currentValue - registers(reg))
    }
  }

  private def handle_sub_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(reg) -= registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val toAdd = memory.readInt(addr)
        registers(reg) += toAdd
    }
  }

  private def handle_sub_imm32_rm32(): Unit = {
    val (mod, _, rm) = parseModRM(nextByte())
    mod match {
      case MOD_REG_DIRECT =>
        registers(rm) = nextInt()
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        val toSubtract = nextInt()
        memory.writeInt(addr, currentValue - toSubtract)
    }
  }

  private def handle_dec_r32(index: Int): Unit = {
    registers(index) -= 1
  }

  private def handle_inc_r32(index: Int): Unit = {
    registers(index) += 1
  }

  private def handle_neg_rm32(mod: Int, rm: Int): Unit = {
    mod match {
      case MOD_REG_DIRECT => registers(rm) = -registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, -currentValue)
    }
  }

  private def handle_mul_rm32(mod: Int, rm: Int): Unit = {
    val op1 = zeroExtendInt(registers(EAX)) // EAX is the implicit first operand for MUL

    val op2 = mod match {
      case MOD_REG_DIRECT =>
        zeroExtendInt(registers(rm)) // zero extend so the ops are treated as unsigned
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        zeroExtendInt(memory.readInt(addr))
    }

    val product = op1 * op2
    registers(EAX) = product & 0xFFFFFFFF // 32 lowest order bits
    registers(EDX) = product >>> 32 // 32 highest order bits
  }

  private def handle_imul_rm32(mod: Int, rm: Int): Unit = {
    val op1 = registers(EAX).toLong  // EAX is the implicit first operand for MUL

    val op2 = mod match {
      case MOD_REG_DIRECT =>
        registers(rm).toLong
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.readInt(addr).toLong
    }

    val product = op1 * op2
    registers(EAX) = product & 0xFFFFFFFF // 32 lowest order bits
    registers(EDX) = product >> 32 // 32 highest order bits
  }

  private def handle_div_rm32(mod: Int, rm: Int): Unit = {
    val lowBits = registers(EAX).toLong
    val highBits = registers(EDX).toLong
    val dividend = (highBits << 32) | lowBits

    val divisor = mod match {
      case MOD_REG_DIRECT =>
        registers(rm)
      case _ =>
        val addr = computeEffectiveAddress(mod, rm)
        memory.readInt(addr)
    }

    val quotient = dividend / divisor
    val remainder = dividend % divisor

    if (quotient > Int.MaxValue) {
      throw new ArithmeticException("Divide error: quotient overflow")
    }
    if (remainder > Int.MaxValue) {
      throw new ArithmeticException("Divide error: remainder overflow")
    }

    registers(EAX) = quotient.toInt
    registers(EDX) = remainder.toInt
  }

  private def handlePushReg(index: Int): Unit = {
    registers(ESP) -= 4
    memory.writeInt(registers(ESP), registers(index))
  }

  private def handleJmpRel32(): Unit = {
    val rel = nextInt()
    eip += rel
  }

  private def handleJmpRel8(): Unit = {
    val rel = nextByte()
    eip += rel
  }

  /** Opcode Group Handlers * */

  private def handle_opcode_0F(): Unit = {
    //TODO: This is wrong
    val next = nextByte() & 0xFF

    next match {
      case Opcodes.JE_REL32_2 =>
        val rel = memory.readInt(eip)
        if ( /* zero flag set */ false) eip += 4 + rel else eip += 4

      case _ =>
        throw new NotImplementedError(f"Unknown 0F opcode: 0F $next%02X")
    }
  }

  private def handle_opcode_F7(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    reg match {
      case NEG_RM32_REG => handle_neg_rm32(mod, rm)
      case MUL_RM32_REG => handle_mul_rm32(mod, rm)
      case IMUL_RM_32_REG => handle_mul_rm32(mod, rm)
    }
  }

  def updateFlagsArithmetic
  (a: Int, b: Int, op: (Int, Int) => Int,
   detectCarry: (Int, Int, Int) => Boolean,
   detectOverflow: (Int, Int, Int) => Boolean,
   detectAdjust: (Int, Int, Int) => Boolean): Int =
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
  def addWithFlags(a: Int, b: Int): Int = {
    updateFlagsArithmetic(
      a, b, _ + _,
      (a, b, res) => (a & UINT32_MAX_LONG) + (b & UINT32_MAX_LONG) > UINT32_MAX_LONG, // cf check
      (a, b, res) => ((a ^ res) & (b ^ res) & SIGN_BIT_MASK) != 0, // of check
      (a, b, res) => ((a ^ b ^ res) & 0x10) != 0 // af check
    )
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
    case (MOD_IND_ADDR_8_DISP, _) => nextByte().toInt
    case (MOD_IND_ADDR_32_DISP, _) => nextInt()
    case (_, _) => 0
  }

  private def computeEffectiveAddress(mod: Int, rm: Int): Int = rm match {
    case RM_SIB_BYTE =>
      val (scale, index, base) = parseSIB(nextByte())
      val displacement = readDisplacement(mod, rm)
      val addr = computeSIB(scale, index, base) + displacement
      addr
    case RM_ABSOLUTE_ADDRESS =>
      readDisplacement(mod, rm) //displacement returns address in this case
    case _ => //register + displacement
      val displacement = readDisplacement(mod, rm)
      registers(rm) + displacement
  }

  /** Extract the mod, register, and rm bits from the ModRM byte * */
  private def parseModRM(modRM: Byte): (Int, Int, Int) = {
    ((modRM >> 6) & 0b11, (modRM >> 3) & 0b111, modRM & 0b111)
  }

  /** Extract the scale, index, and base bits from SIB byte * */
  private def parseSIB(sib: Byte): (Int, Int, Int) = {
    ((sib >> 6) & 0b11, (sib >> 3) & 0b111, sib & 0b111)
  }

  def zeroExtendInt(i: Int): Long = i & UINT32_MAX

  /** Used to indicate the unsigned sum is greater than what can fit in the destination. */
  def addCarry(op1: Int, op2: Int): Boolean = {
    // This removes the sign bit so that the values are treated as unsigned
    (op1.toLong & UINT32_MAX_LONG) + (op2.toLong & UINT32_MAX_LONG) > UINT32_MAX_LONG
  }

  def hasEvenParity(value: Int): Boolean = (Integer.bitCount(value) & 1) == 0
  def isZero(value: Int): Boolean = value == 0

  def isNegative(value: Int): Boolean = value & SIGN_BIT_MASK != 0

}