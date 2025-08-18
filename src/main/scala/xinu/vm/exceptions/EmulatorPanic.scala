package xinu.vm.exceptions

case class EmulatorPanic(msg: String) extends RuntimeException(msg)
