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
    val imm = memory.readInt(eip + 1)
    registers(ECX) = imm
    eip += 5
  }
  private def handleMovImm32Edx(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EDX) = imm
    eip += 5
  }
  private def handleMovImm32Ebx(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EBX) = imm
    eip += 5
  }
  private def handleMovImm32Esp(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(ESP) = imm
    eip += 5
  }
  private def handleMovImm32Ebp(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EBP) = imm
    eip += 5
  }
  private def handleMovImm32Esi(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(ESI) = imm
    eip += 5
  }
  private def handleMovImm32Edi(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EDI) = imm
    eip += 5
  }

  /* MOV r, r */
  private def handleMov_rm32_r32(): Unit = {
    val (mod: Int, reg: Int, rm: Int) = parseModRM(memory.readByte(eip + 1))
    eip += 2 //increment for instruction byte and modRM byte

    if (mod == REG_DIRECT) {
      registers(reg) = registers(rm)
    }
    else {
      val (displacement: Int, bytesRead: Int) = mod match {
        case IND_ADDR_8_DISP => (memory.readByte(eip).toInt, 1)
        case IND_ADDR_32_DISP => (memory.readInt(eip), 4)
        case _ => (0, 0)
      }
      eip += bytesRead

      if (rm == ABSOLUTE_ADDRESS) {
        val addr = memory.readInt(eip)
        eip += 4

        registers(reg) = memory.readInt(addr)
      }
      else if (rm == RM_SIB_BYTE) {
        val (scale, index, base) = parseSIB(memory.readByte(eip))
        eip += 1

        val addr = (base + (index * scale)) + displacement
        registers(reg) = memory.readInt(addr)
      }
      else {
        val addr = registers(rm) + displacement
        registers(reg) = memory.readInt(addr)
      }
    }
  }
  private def handleMov_r32_rm32(): Unit = {
    val (mod: Int, reg: Int, rm: Int) = parseModRM(memory.readByte(eip + 1))
    eip += 2 //increment for instruction byte and modRM byte

    if (mod == REG_DIRECT) {
      registers(rm) = registers(reg)
    }
    else {
      val (displacement: Int, bytesRead: Int) = mod match {
        case IND_ADDR_8_DISP => (memory.readByte(eip).toInt, 1)
        case IND_ADDR_32_DISP => (memory.readInt(eip), 4)
        case _ => (0, 0)
      }
      eip += bytesRead

      if (rm == ABSOLUTE_ADDRESS) {
        val addr = memory.readInt(eip)
        eip += 4

        memory.writeInt(addr, registers(reg))
      }
      else if (rm == RM_SIB_BYTE) {
        val (scale, index, base) = parseSIB(memory.readByte(eip))
        eip += 1

        val addr = (base + (index * scale)) + displacement
        memory.writeInt(addr, registers(reg))
      }
      else {
        val addr = registers(rm) + displacement
        memory.writeInt(addr, registers(reg))
      }
    }
  }

  /* TODO: Look at special SIB (and potentially modrm) cases */
  /* LEA m, r32 */
  private def handleLea_m_r32(): Unit = {
    //get modRM byte values
    val (mod: Int, reg: Int, rm: Int) = parseModRM(memory.readByte(eip + 1))
    eip += 2  //increment for instruction and modRM bytes

    val (displacement: Int, bytesRead: Int) = mod match {
      case IND_ADDR_8_DISP => (memory.readByte(eip).toInt, 1)
      case IND_ADDR_32_DISP => (memory.readInt(eip), 4)
      case _ => (0, 0)
    }
    eip += bytesRead

    //if SIB byte present
    if (rm == RM_SIB_BYTE) {
      //SIB Byte
      val (scale, index, base) = parseSIB(memory.readByte(eip))
      eip += 1

      val addr = base + (index * scale) + displacement
      registers(reg) = addr
    }
    else if (mod == IND_ADDR_NO_DISP) {
      //absolute address
      val addr = memory.readInt(eip + 2)
      registers(reg) = addr
      eip += 6
    }
    else if (mod == IND_ADDR_8_DISP) {
      val displacement = memory.readByte(eip + 2)
      val addr = registers(rm) + displacement
      registers(reg) = addr
      eip += 3
    }
    else if (mod == IND_ADDR_32_DISP) {
      val displacement = memory.readInt(eip + 2)
      val addr = registers(rm) + displacement
      registers(reg) = addr
      eip += 6
    }
    else {
      //reg-direct, should not occur, throw exception
      throw new InvalidOperandException(
        s"Invalid addressing mode for lea: register-direct (mod = 11) at EIP = 0x${eip.toHexString}")
    }
  }

  /* Add Handlers -------------------------------------------------------------------------- */

  private def handleAddImm32Eax(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(0) += imm
    eip += 5
  }
  private def handleAdd_r32_rm32(): Unit = {
    val (mod, reg, rm) = parseModRM(memory.readByte(eip + 1))

    if (mod == REG_DIRECT) {
      registers(rm) += registers(reg)
    }

  }


  private def handleIncReg(index: Int): Unit = {
    registers(index) += 1
    eip += 1
  }

  private def handlePushReg(index: Int): Unit = {
    registers(ESP) -= 4
    memory.writeInt(registers(ESP), registers(index))
    eip += 1
  }

  private def handleJmpRel32(): Unit = {
    val rel = memory.readInt(eip + 1)
    eip += 5 + rel
  }

  private def handleJmpRel8(): Unit = {
    val rel = memory.readByte(eip + 1).toByte // Sign-extended
    eip += 2 + rel
  }

  private def handleOpcode0F(): Unit = {
    val next = memory.readByte(eip + 1) & 0xFF
    next match {
      case Opcodes.JE_REL32_2 =>
        val rel = memory.readInt(eip + 2)
        if (/* zero flag set */ false) eip += 6 + rel else eip += 6

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
}