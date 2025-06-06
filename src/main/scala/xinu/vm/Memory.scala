package xinu.vm

class Memory(size: Int) {

  private val ram = Array.fill(size)(0.toByte)  //should this be private or public?

  def readByte(addr: Int): Byte = {
    ram(addr);
  }

  def readInt(addr: Int): Int = {
    (0 until 4).map(i => readByte(addr + i) << (i * 8)).reduce(_ | _)
  }

  def writeInt(addr: Int, value: Int): Unit = {
    (0 until 4).foreach(i => ram(addr + i) = ((value >> (i * 8)) & 0xFF).toByte)
  }

}
