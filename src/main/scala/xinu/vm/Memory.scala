package xinu.vm

class Memory(size: Int) {

  private val memSize = size
  val ram: Array[Byte] = Array.fill(size)(0.toByte)
  private val stackSize = size / 8

  /** Masks result since Scala interprets byte as signed */
  def readByte(addr: Int): Int = {
    ram(addr) & 0xFF
  }

  def readWord(addr: Int): Short = {
    (0 until 2).map(i => readByte(addr + i) << (i * 8)).reduce(_ | _).toShort
  }

  def readInt(addr: Int): Int = {
    (0 until 4).map(i => readByte(addr + i) << (i * 8)).reduce(_ | _)
  }

  def writeByte(addr: Int, value: Byte): Unit = {
    ram(addr) = value
  }

  def writeInt(addr: Int, value: Int): Unit = {
    (0 until 4).foreach(i => ram(addr + i) = ((value >> (i * 8)) & 0xFF).toByte)
  }

  def overflowsStack(size: Int, esp: Int): Boolean = {
    esp + size > stackSize
  }

  def underflowsStack(size: Int, esp: Int): Boolean = {
    esp - size < 0
  }

  def getMemSize: Int = memSize

}
