package xinu.vm

import xinu.vm.Opcodes._
import xinu.vm.exceptions._

class CPU (memory: Memory) {

  private val registers = Array.fill(8)(0)

  private val EAX = 0
  private val ECX = 1
  private val EDX = 2
  private val EBX = 3
  private val ESP = 4
  private val EBP = 5
  private val ESI = 6
  private val EDI = 7

  private var flags = 0
  private var eip = 0

  private type InstructionHandler = () => Unit
  private val handlers: Array[InstructionHandler] = Array.fill(256)(unimplementedOpcode)

  def step(): Unit = {
    val opcode = nextByte()
    handlers(opcode)()
  }

  // === Handler Initialization ===
  def initHandlers(): Unit = {
    // MOV r32, imm32
    handlers(Opcodes.MOV_IMM32_EAX) = handle_mov_imm32_eax
    handlers(Opcodes.MOV_IMM32_ECX) = handle_mov_imm32_ecx
    handlers(Opcodes.MOV_IMM32_EDX) = handle_mov_imm32_edx
    handlers(Opcodes.MOV_IMM32_EBX) = handle_mov_imm32_ebx
    handlers(Opcodes.MOV_IMM32_ESP) = handle_mov_imm32_esp
    handlers(Opcodes.MOV_IMM32_EBP) = handle_mov_imm32_ebp
    handlers(Opcodes.MOV_IMM32_ESI) = handle_mov_imm32_esi
    handlers(Opcodes.MOV_IMM32_EDI) = handle_mov_imm32_edi

    // MOV rm32, r32
    handlers(Opcodes.MOV_RM32_R32) = handle_mov_rm32_r32

    // MOV r32, rm32
    handlers(Opcodes.MOV_R32_RM32) = handle_mov_r32_rm32

    // LEA r32, m
    handlers(Opcodes.LEA_M_R32) = handle_lea_m_r32

    // ADD
    handlers(Opcodes.ADD_IMM32_EAX) = handle_add_imm32_eax

    handlers(Opcodes.ADD_R32_RM32) = handle_add_r32_rm32
    handlers(Opcodes.ADD_RM32_R32) = handle_add_rm32_r32
    handlers(Opcodes.ADD_IMM32_RM32) = handle_add_imm32_rm32

    // SUB
    handlers(Opcodes.SUB_IMM32_EAX) = handle_sub_imm32_eax
    handlers(Opcodes.SUB_R32_RM32) = handle_sub_r32_rm32

    // INC r32 (0x40–0x47)
    for (i <- 0 to 7) {
      handlers(Opcodes.INC_R32_BASE + i) = () => handleIncReg(i)
    }

    // PUSH r32 (0x50–0x57)
    for (i <- 0 to 7) {
      handlers(Opcodes.PUSH_R32_BASE + i) = () => handlePushReg(i)
    }

    // JMP
    handlers(Opcodes.JMP_REL32) = handleJmpRel32
    handlers(Opcodes.JMP_REL8)  = handleJmpRel8

    // 0x0F escape prefix
    handlers(0x0F) = handleOpcode0F
  }

  // === Fallback ===
  private def unimplementedOpcode(): Unit = {
    val op = memory.readByte(eip) & 0xFF
    throw new NotImplementedError(f"Unimplemented opcode: 0x$op%02X at EIP=0x$eip%08X")
  }


  /* -------------------- Handlers ------------------------- */

  /* MOV r, imm32 */
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

  /* MOV r, r */
  private def handle_mov_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    if (mod == MOD_REG_DIRECT) {
      registers(reg) = registers(rm)
    }
    else {
      val addr = computeEffectiveAddress(mod, rm)
      registers(reg) = memory.readInt(addr)
    }
  }

  private def handle_mov_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    if (mod == MOD_REG_DIRECT) {
      registers(rm) = registers(reg)
    }
    else {
      val addr = computeEffectiveAddress(mod, rm)
      memory.writeInt(addr, registers(reg))
    }
  }

  /* LEA m, r32 */
  private def handle_lea_m_r32(): Unit = {
    val (mod: Int, reg: Int, rm: Int) = parseModRM(nextByte())
    val addr = computeEffectiveAddress(mod, rm)
    registers(reg) = addr
  }

  /* Add Handlers -------------------------------------------------------------------------- */

  private def handle_add_imm32_eax(): Unit = {
    val imm = nextInt()
    registers(EAX) += imm
  }

  private def handle_add_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    if (mod == MOD_REG_DIRECT) {
      registers(rm) += registers(reg)
    }
    else {
      val addr = computeEffectiveAddress(mod, rm)
      val currentValue = memory.readInt(addr)
      memory.writeInt(addr, currentValue + registers(reg))
    }
  }

  private def handle_add_rm32_r32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    if (mod == MOD_REG_DIRECT) {
      registers(reg) += registers(rm)
    }
    else {
      val addr = computeEffectiveAddress(mod, rm)
      registers(reg) += memory.readInt(addr)
    }
  }

  private def handle_add_imm32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    if (mod == MOD_REG_DIRECT) {
      registers(rm) += nextInt()
    }
    else {
      val addr = computeEffectiveAddress(mod, rm)
      val currentValue = memory.readInt(addr)
      /* Fetch the value we are adding */
      val toAdd = nextInt()
      /* Write the new value */
      memory.writeInt(addr, currentValue + toAdd)
    }
  }

  private def handle_sub_imm32_eax(): Unit = {
    val imm = nextInt()
    registers(EAX) -= imm
  }

  private def handle_sub_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(nextByte())

    if (mod == MOD_REG_DIRECT) {
      registers(rm) -= registers(reg)
    }
    else {
      val addr = computeEffectiveAddress(mod, rm)
      val currentValue = memory.readInt(addr)
      memory.writeInt(addr, currentValue - registers(reg))
    }
  }

  private def handleIncReg(index: Int): Unit = {
    registers(index) += 1
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

  private def handleOpcode0F(): Unit = {
    val next = nextByte() & 0xFF

    next match {
      case Opcodes.JE_REL32_2 =>
        val rel = memory.readInt(eip)
        if (/* zero flag set */ false) eip += 4 + rel else eip += 4

      case _ =>
        throw new NotImplementedError(f"Unknown 0F opcode: 0F $next%02X")
    }
  }

  /* Extract the mod, register, and rm bits from the ModRM byte */
  private def parseModRM(modRM: Byte): (Int, Int, Int) = {
    ((modRM >> 6) & 0b11, (modRM >> 3) & 0b111, modRM & 0b111)
  }

  /* Extract the scale, index, and base bits from SIB byte */
  private def parseSIB(sib: Byte): (Int, Int, Int) = {
    ((sib >> 6) & 0b11, (sib >> 3) & 0b111, sib & 0b111)
  }

  /* Computes the effective result of SIB. Does not include displacement. */
  private def computeSIB(scale: Int, index: Int, base: Int) = {
    val trueScale = 1 << scale //converts scale bits into the true scale
    val result = (base, index) match {
      case (SIB_BASE_DISABLED, SIB_INDEX_DISABLED) => 0
      case (SIB_BASE_DISABLED, _) => registers(index) * trueScale
      case (_, SIB_INDEX_DISABLED) => registers(base)
      case (_,_) => registers(base) + (registers(index) * trueScale)
    }
    result
  }

  /* Reads the displacement, does not increment instruction pointer, eip must be positioned prior */
  private def readDisplacement(mod: Int, rm: Int): Int = (mod, rm) match {
    case (MOD_IND_ADDR_NO_DISP, RM_ABSOLUTE_ADDRESS) => nextInt()
    case (MOD_IND_ADDR_8_DISP, _)  => nextByte().toInt
    case (MOD_IND_ADDR_32_DISP, _) => nextInt()
    case (_,_) => 0
  }

  private def computeEffectiveAddress(mod: Int, rm: Int): Int = {
    if (rm == RM_SIB_BYTE) {
      val (scale, index, base) = parseSIB(nextByte())
      val displacement = readDisplacement(mod, rm)
      val addr = computeSIB(scale, index, base) + displacement
      addr
    }
    else {
      val displacement = readDisplacement(mod, rm)
      val addr = rm match {
        case RM_ABSOLUTE_ADDRESS => displacement
        case _ =>
          registers(rm) + displacement
      }
      addr
    }
  }

  private def nextByte(): Byte = {
    val byte = memory.readByte(eip)
    eip += 1
    byte
  }

  private def nextInt(): Int = {
    val integer = memory.readInt(eip)
    eip += 4
    integer
  }
}