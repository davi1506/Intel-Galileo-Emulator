package xinu.vm.exceptions

class InvalidOpcodeException(opcode: Int)
  extends RuntimeException(f"Invalid opcode: 0x$opcode%02X")
