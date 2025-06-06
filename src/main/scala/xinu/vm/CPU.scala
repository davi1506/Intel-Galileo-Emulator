package xinu.vm

import xinu.vm.Opcodes._

class CPU (memory: Memory) {

  val registers = Array.fill(8)(0)

  val EAX = 0
  val ECX = 1
  val EDX = 2
  val EBX = 3
  val ESP = 4
  val EBP = 5
  val ESI = 6
  val EDI = 7

  var flags = 0
  var eip = 0

  type InstructionHandler = () => Unit
  val handlers: Array[InstructionHandler] = Array.fill(256)(unimplementedOpcode)

  def step(): Unit = {
    val opcode = memory.readByte(eip)
    handlers(opcode)()
  }

  // === Handler Initialization ===
  def initHandlers(): Unit = {
    // MOV r32, imm32
    handlers(Opcodes.MOV_EAX_IMM32) = handleMovEaxImm32
    handlers(Opcodes.MOV_ECX_IMM32) = handleMovEcxImm32
    handlers(Opcodes.MOV_EDX_IMM32) = handleMovEdxImm32
    handlers(Opcodes.MOV_EBX_IMM32) = handleMovEbxImm32
    handlers(Opcodes.MOV_ESP_IMM32) = handleMovEspImm32
    handlers(Opcodes.MOV_EBP_IMM32) = handleMovEbpImm32
    handlers(Opcodes.MOV_ESI_IMM32) = handleMovEsiImm32
    handlers(Opcodes.MOV_EDI_IMM32) = handleMovEdiImm32

    // ADD
    handlers(Opcodes.ADD_EAX_IMM32) = handleAddEaxImm32

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
  private def handleMovEaxImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EAX) = imm
    eip += 5
  }
  private def handleMovEcxImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(ECX) = imm
    eip += 5
  }
  private def handleMovEdxImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EDX) = imm
    eip += 5
  }
  private def handleMovEbxImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EBX) = imm
    eip += 5
  }
  private def handleMovEspImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(ESP) = imm
    eip += 5
  }
  private def handleMovEbpImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EBP) = imm
    eip += 5
  }
  private def handleMovEsiImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(ESI) = imm
    eip += 5
  }
  private def handleMovEdiImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(EDI) = imm
    eip += 5
  }

  /* MOV r, r */
  private def handleMov_rm32_r32(): Unit = {
    val modRM = memory.readByte(eip + 1)
    val mod = (modRM >> 6) & 0b11
    val reg = (modRM >> 3) & 0b111
    val rm = modRM & 0b111

    if (mod == REG_DIRECT) {
      registers(reg) = registers(rm)
      eip += 1
    }
    else if (mod == IND_ADDR_NO_DISP) {
      if (rm == ABSOLUTE_ADDRESS) {
        val addr = memory.readInt(eip + 2)
        registers(reg) = memory.readInt(addr)
        eip += 5
      }
      else {
        val addr = registers(rm)
        registers(reg) = memory.readInt(addr)
        eip += 5
      }
    }
    else if (mod == IND_ADDR_8_DISP) {
      val displacement = memory.readByte(eip + 2)
      val addr = registers(rm) + displacement
      registers(reg) = memory.readInt(addr)
      eip += 2
    }
    else {
      val displacement = memory.readInt(eip + 2)
      val addr = registers(rm) + displacement
      registers(reg) = memory.readInt(addr)
      eip += 5
    }
  }

  /* Add Handlers -------------------------------------------------------------------------- */

  private def handleAddEaxImm32(): Unit = {
    val imm = memory.readInt(eip + 1)
    registers(0) += imm
    eip += 5
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
}