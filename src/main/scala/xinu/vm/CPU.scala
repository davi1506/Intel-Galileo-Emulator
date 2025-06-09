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
    val opcode = memory.readByte(eip)
    handlers(opcode)()
  }

  // === Handler Initialization ===
  def initHandlers(): Unit = {
    // MOV r32, imm32
    handlers(Opcodes.MOV_IMM32_EAX) = handleMovImm32Eax
    handlers(Opcodes.MOV_IMM32_ECX) = handleMovImm32Ecx
    handlers(Opcodes.MOV_IMM32_EDX) = handleMovImm32Edx
    handlers(Opcodes.MOV_IMM32_EBX) = handleMovImm32Ebx
    handlers(Opcodes.MOV_IMM32_ESP) = handleMovImm32Esp
    handlers(Opcodes.MOV_IMM32_EBP) = handleMovImm32Ebp
    handlers(Opcodes.MOV_IMM32_ESI) = handleMovImm32Esi
    handlers(Opcodes.MOV_IMM32_EDI) = handleMovImm32Edi

    // MOV rm32, r32
    handlers(Opcodes.MOV_RM32_R32) = handleMov_rm32_r32

    // MOV r32, rm32
    handlers(Opcodes.MOV_R32_RM32) = handleMov_r32_rm32

    // LEA r32, m
    handlers(Opcodes.LEA_M_R32) = handleLea_m_r32

    // ADD
    handlers(Opcodes.ADD_IMM32_EAX) = handleAddImm32Eax

    handlers(Opcodes.ADD_R32_RM32) = handleAdd_r32_rm32

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
  private def handleMovImm32Eax(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EAX) = imm
    eip += 5
  }
  private def handleMovImm32Ecx(): Unit = {
    eip += 1

    val imm = memory.readInt(eip)
    eip += 4

    registers(ECX) = imm
  }
  private def handleMovImm32Edx(): Unit = {
    eip += 1

    val imm = memory.readInt(eip)
    eip += 4

    registers(EDX) = imm
  }
  private def handleMovImm32Ebx(): Unit = {
    eip += 1

    val imm = memory.readInt(eip)
    eip += 4

    registers(EBX) = imm
  }
  private def handleMovImm32Esp(): Unit = {
    eip += 1

    val imm = memory.readInt(eip)
    eip += 4

    registers(ESP) = imm
  }
  private def handleMovImm32Ebp(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EBP) = imm
    eip += 5
  }
  private def handleMovImm32Esi(): Unit = {
    eip += 1

    val imm = memory.readInt(eip)
    eip += 4

    registers(ESI) = imm
  }
  private def handleMovImm32Edi(): Unit = {
    eip += 1

    val imm = memory.readInt(eip + 1)
    eip += 4

    registers(EDI) = imm
  }

  /* MOV r, r */
  private def handleMov_rm32_r32(): Unit = {
    eip += 1

    val (mod: Int, reg: Int, rm: Int) = parseModRM(memory.readByte(eip))
    eip += 1

    if (mod == MOD_REG_DIRECT) {
      registers(reg) = registers(rm)
    }
    else {
      if (rm == RM_SIB_BYTE) {
        val (scale, index, base) = parseSIB(memory.readByte(eip))
        eip += 1

        val (displacement: Int, bytesRead: Int) = readDisplacement(mod)
        eip += bytesRead

        val addr = computeSIB(scale, index, base) + displacement
        registers(reg) = memory.readInt(addr)
      }
      else {
        val (displacement: Int, bytesRead: Int) = readDisplacement(mod)
        eip += bytesRead

        val addr = rm match {
          case RM_ABSOLUTE_ADDRESS => displacement
          case _ => registers(rm) + displacement
        }
        registers(reg) = memory.readInt(addr)
      }
    }
  }
  private def handleMov_r32_rm32(): Unit = {
    eip += 1

    val (mod: Int, reg: Int, rm: Int) = parseModRM(memory.readByte(eip))
    eip += 1

    if (mod == MOD_REG_DIRECT) {
      registers(rm) = registers(reg)
    }
    else {
      if (rm == RM_SIB_BYTE) {
        val (scale, index, base) = parseSIB(memory.readByte(eip))
        eip += 1

        val (displacement, bytesRead) = readDisplacement(mod)
        eip += bytesRead

        val addr = computeSIB(scale, index, base) + displacement
        memory.writeInt(addr, registers(reg))
      }
      else {
        val (displacement, bytesRead) = readDisplacement(mod)
        eip += bytesRead

        val addr = rm match {
          case RM_ABSOLUTE_ADDRESS => displacement
          case _ => registers(rm) + displacement
        }

        memory.writeInt(addr, registers(reg))
      }
    }
  }

  /* LEA m, r32 */
  private def handleLea_m_r32(): Unit = {
    eip += 1

    val (mod: Int, reg: Int, rm: Int) = parseModRM(memory.readByte(eip))
    eip += 1

    //if SIB byte present
    if (rm == RM_SIB_BYTE) {
      val (scale, index, base) = parseSIB(memory.readByte(eip))
      eip += 1

      val (displacement, bytesRead) = readDisplacement(mod)
      eip += bytesRead

      val addr = computeSIB(scale, index, base) + displacement
      registers(reg) = addr
    }
    else {
      val (displacement, bytesRead) = readDisplacement(mod)
      eip += bytesRead

      val addr = rm match {
        case RM_ABSOLUTE_ADDRESS => displacement  //with absolute addressing, the displacement is the address
        case _ => registers(rm) + displacement
      }
      registers(reg) = addr
    }
  }

  /* Add Handlers -------------------------------------------------------------------------- */

  private def handleAddImm32Eax(): Unit = {
    eip += 1

    val imm = memory.readInt(eip)
    eip += 4

    registers(EAX) += imm
  }
  private def handleAdd_r32_rm32(): Unit = {
    eip += 1

    val (mod, reg, rm) = parseModRM(memory.readByte(eip))
    eip += 1

    if (mod == MOD_REG_DIRECT) {
      registers(rm) += registers(reg)
    }
    else {
      if (rm == RM_SIB_BYTE) {
        val (scale, index, base) = parseSIB(memory.readByte(eip))
        eip += 1

        val (displacement, bytesRead) = readDisplacement(mod)
        eip += bytesRead

        val addr = computeSIB(scale, index, base) + displacement
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, currentValue + registers(reg))
      }
      else {
        val (displacement, bytesRead) = readDisplacement(mod)
        eip += bytesRead

        val addr = rm match {
          case RM_ABSOLUTE_ADDRESS => displacement
          case _ => registers(rm) + displacement
        }
        val currentValue = memory.readInt(addr)
        memory.writeInt(addr, currentValue + registers(reg))
      }
    }
  }

  private def handleIncReg(index: Int): Unit = {
    eip += 1
    registers(index) += 1
  }

  private def handlePushReg(index: Int): Unit = {
    eip += 1
    registers(ESP) -= 4
    memory.writeInt(registers(ESP), registers(index))
  }

  private def handleJmpRel32(): Unit = {
    eip += 1
    val rel = memory.readInt(eip)
    eip += 4 + rel
  }

  private def handleJmpRel8(): Unit = {
    eip += 1
    val rel = memory.readByte(eip).toByte // Sign-extended
    eip += 1 + rel
  }

  private def handleOpcode0F(): Unit = {
    eip += 1
    val next = memory.readByte(eip) & 0xFF
    eip += 1

    next match {
      case Opcodes.JE_REL32_2 =>
        val rel = memory.readInt(eip)
        if (/* zero flag set */ false) eip += 4 + rel else eip += 4

      case _ =>
        throw new NotImplementedError(f"Unknown 0F opcode: 0F ${next}%02X")
    }
  }

  private def parseModRM(modRM: Byte): (Int, Int, Int) = {
    ((modRM >> 6) & 0b11, (modRM >> 3) & 0b111, modRM & 0b111)
  }
  private def parseSIB(sib: Byte): (Int, Int, Int) = {
    ((sib >> 6) & 0b11, (sib >> 3) & 0b111, sib & 0b111)
  }
  /* Computes the effective result of SIB. Does not include displacement. */
  private def computeSIB(scale: Int, index: Int, base: Int) = {
    val result = (base, index) match {
      case (SIB_BASE_DISABLED, SIB_INDEX_DISABLED) => 0
      case (SIB_BASE_DISABLED, _) => index * scale
      case (_, SIB_INDEX_DISABLED) => base
      case (_,_) => (base + (index * scale))
    }
    result
  }
  /* Reads the displacement, does not increment */
  private def readDisplacement(mod: Int): (Int, Int) = mod match {
    case MOD_IND_ADDR_8_DISP => (memory.readByte(eip).toInt, 1)
    case MOD_IND_ADDR_32_DISP => (memory.readInt(eip), 4)
    case _ => (0, 0)
  }
}