package xinu.vm

import java.nio.file.{Files, Paths}

object Main {

  def main(args: Array[String]): Unit = {
    start()
  }
  private def start(): Unit = {
    val size = 10000000
    val memory = new Memory(size)
    val program: Array[Byte] = Files.readAllBytes(Paths.get("/home/adam/IdeaProjects/Virtual Machine/src/main/scala/xinu/vm/xinu.bin"))
    Array.copy(program, 0, memory.ram, 0x100000, program.length)

    val cpu = new CPU(memory)
    cpu.initHandlers()
    while (true) {
      cpu.step()
    }
  }
}

//16e68