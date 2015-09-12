import java.io.{PrintWriter, File}

import scala.annotation.tailrec

/**
 * Created by Hywel on 4/12/15.
 */

object FileHelper {
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }
}

class Console(val cartridge:Cartridge, val cpu: CPU, val ram:Array[Int], val mapper:Mapper, val ppu:PPU, val apu:APU, val controller1:Controller, val controller2:Controller){
  def Start() = {
    println("s: Step into CPU r: Run until BANG! l: Log output of CPU to file q: Quit")

    while (true) scala.Console.in.readLine() match {
      case input if input == "r" => Run()
      case input if input == "s" => print(cpu); Step()
      case input if input == "q" => System.exit(1)
      case input if input == "l" => FileHelper.printToFile(new File("nestest.log")) { p => Run(p)}
      case _ => println(s"Invalid input, only r (Run) / s (Step) / l (Log) / q (Quit) commands are available")
    }
  }

  private def Step(): Unit ={
    val cpuCycles = cpu.Step()
    val ppuCycles = cpuCycles * 3

    for (i <- 0 to ppuCycles - 1) {
      ppu.Step(cpu)
      mapper.Step()
    }

    for (i <- 0 to cpuCycles - 1) {
      apu.Step()
    }
  }
  @tailrec
  private def Run():Int = {
    Step()
    Run()
  }

  private def Run(printWriter: PrintWriter):Unit = {
    try{
      for(i <- 0 to 9100){
        printWriter.println(cpu)
        Step()
      }
    } catch {
      case e:Throwable => println(e.printStackTrace()); System.exit(99)
    }
    //Run(printWriter)
  }
}

object Console
{
  def main(args: Array[String])
  {
    if (args.length != 1) {
      System.err.println("Usage: nescala rom_file")
      System.exit(1)
    }

    val filename = args(0)
    // Load as Cartridge
    val cartridge = Cartridge(filename)
    val ram = new Array[Int](2048)
    val controller1 = Controller()
    val controller2 = Controller()
    val mapper = Mapper(cartridge)
    val apu = new APU()
    val ppu = PPU(cartridge, mapper)
    val cpu = CPU(ram, ppu, apu, controller1, controller2, mapper)

    val Console = new Console(cartridge, cpu, ram, mapper, ppu, apu, controller1, controller2)

    println(cartridge.Header)
    print("\n")
    println(cartridge)

    Console.Start()
  }
}
