package nescala

import java.awt.image.BufferedImage
import java.io.{File, PrintWriter}

import ui.Audio

import scala.annotation.tailrec

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

final class Console(val cartridge:Cartridge, val cpu: CPU, val ram:Array[Int], val mapper:Mapper, val ppu:PPU, val apu:APU, val controller1:Controller, val controller2:Controller) {
  private def Step(): Long ={
    val cpuCycles = cpu.Step()
    val ppuCycles = cpuCycles * 3

    for (i <- 0L until ppuCycles) {
      ppu.Step(cpu.triggerNMI)
      mapper.Step()
    }

    for (i <- 0L until cpuCycles) {
      apu.Step(cpu.memory.Read, cpu.triggerIRQ)
    }

    cpuCycles
  }

  def StepFrame(): Long = {
    var cpuCycles = 0L
    val frame = ppu.frame
    while (frame == ppu.frame) {
      cpuCycles += Step()
    }
    cpuCycles
  }

  def StepSeconds(seconds:Double):Unit = {
    var cycles = CPU.frequency * seconds
    while (cycles > 0) {
      cycles = cycles - Step()
    }
  }

  def VideoBuffer(): BufferedImage = ppu.front

  @tailrec
  def Run():Unit = {
    Step()
    Run()
  }

  @tailrec
  private def Run(printWriter: PrintWriter):Unit = {
    try{
        printWriter.println(cpu)
        Step()
    } catch {
      case e:Throwable => println(e.printStackTrace()); System.exit(99)
    }

    Run(printWriter)
  }

  def SetButtons(buttons:Map[Int, Boolean]) = controller1.SetButtons(buttons)
}

object Console
{
  def main(args: Array[String])
  {
    if (args.length != 1) {
      System.err.println("Usage: nescala file")
      System.exit(1)
    }

    val filename = args(0)
    val console = Console(filename, new Audio)

    start(console)
  }

  def start(console:Console) = {
    println("s: Step into CPU r: Run Ui! l: Log output of CPU to file q: Quit")

    while (true) scala.Console.in.readLine() match {
      case input if input == "r" => console.Run()
      case input if input == "s" => print(console.cpu); console.Step()
      case input if input == "q" => System.exit(1)
      case input if input == "l" => FileHelper.printToFile(new File("nestest.log")) { p => console.Run(p)}
      case _ => println(s"Invalid input, only r (Run) / s (Step) / l (Log) / q (Quit) commands are available")
    }
  }

  def apply(filename:String, audio:Audio): Console = {
    // Load as Cartridge
    val cartridge = Cartridge(filename)
    val ram = Array.fill(2048)(0)
    val controller1 = Controller()
    val controller2 = Controller()
    val mapper = Mapper(cartridge)
    val apu = new APU(audio)
    val ppu = PPU(cartridge, mapper)
    val cpu = CPU(ram, ppu, apu, controller1, controller2, mapper)

    val Console = new Console(cartridge, cpu, ram, mapper, ppu, apu, controller1, controller2)

    println(s"""
            |${cartridge.Header}
            |
            |$cartridge
            """.stripMargin)

    Console
  }
}
