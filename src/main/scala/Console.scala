package nescala

import java.io.PrintWriter

import helpers.File
import ui.Audio
import scala.annotation.tailrec

final class Console(cartridge:Cartridge, val cpu: CPU, ram:Array[Int], mapper:Mapper, val ppu:PPU, apu:APU, val controller1:Controller, val controller2:Controller) {
  private def Step(): Int = {
    val cpuCycles = cpu.Step()
    val ppuCycles = cpuCycles * 3

    for (i <- 0 until ppuCycles) {
      ppu.Step(cpu.triggerNMI)
      mapper.Step(ppu.Cycle, ppu.ScanLine, ppu.ShowBackground, ppu.ShowSprites, cpu.triggerIRQ)
    }

    for (i <- 0 until cpuCycles){
      apu.Step(cpu.memory.Read, cpu.triggerIRQ)
    }

    cpuCycles
  }

  @tailrec
  def StepFrame(cpuCycles:Int = 0, frame:Long = ppu.Frame): Int =
    if (frame == ppu.Frame) StepFrame(cpuCycles + Step())
    else cpuCycles

  def StepSeconds(seconds:Double):Unit = {
    var cycles = (CPU.frequency * seconds).toInt
    while (cycles > 0) cycles = cycles - Step()
  }

  def VideoBuffer() = ppu.Front

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

  def Reset() {
    cpu.Reset()
    ppu.Reset()
  }
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
    println("s: Step into CPU r: Run l: Log output of CPU to file q: Quit")

    while (true) scala.Console.in.readLine() match {
      case input if input == "r" => console.Run()
      case input if input == "s" => print(console.cpu); console.Step()
      case input if input == "q" => System.exit(1)
      case input if input == "l" => File.Writer("nestest.log") { p => console.Run(p)}
      case _ => println(s"Invalid input, only r (Run) / s (Step) / l (Log) / q (Quit) commands are available")
    }
  }

  def apply(filename:String, audio:Audio): Console = {

    val cartridge = Cartridge(filename)
    val ram = Array.fill(2048)(0)
    val controller1 = Controller()
    val controller2 = Controller()
    val mapper = Mapper(cartridge)
    val apu = APU(audio.receive)
    val ppu = PPU(cartridge, mapper)
    val cpu = CPU(ram, ppu, apu, controller1, controller2, mapper)

    val Console = new Console(cartridge, cpu, ram, mapper, ppu, apu, controller1, controller2)

    println(s"""
            |File: $filename
            |$cartridge
            |${cartridge.Header}
            """.stripMargin)

    Console
  }
}
