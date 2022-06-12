package com.owlandrews.nescala

trait Mapper {
  def Mirror:Int
  def Read(address:Int): Int
  def Write(address:Int, value: Int): Unit
  def Step(ppuCycle:Long, ppuScanLine:Long, ppuFlagShowBackground:Int, ppuFlagShowSprites:Int, flagSpriteSize:Int, triggerIRQ: => Unit) : Unit = ()
  val sRamAddress = 0x6000
  val isChr: Int => Boolean = x => x < 0x2000
  val isPrg1: Int => Boolean = x => x >= 0x8000
  val isPrg2:Int => Boolean = x => x >= 0xC000
  val isSRam: Int => Boolean = x => x >= sRamAddress
}

object MirrorMode {
  val Horizontal = 0
  val Vertical   = 1
  val Single0    = 2
  val Single1    = 3
  val Four       = 4
}

object Mapper {
  def apply(cartridge:Cartridge): Mapper = {
    cartridge.Mapper match {
      case 0 | 2 | 71 => Mapper2(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case 1          => Mapper1(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case 3          => Mapper3(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case 4          => Mapper4(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case 7          => Mapper7(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case 9          => Mapper9(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case 23         => Mapper23(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case 25         => Mapper25(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case unsupported => throw new Exception(s"Unhandled mapper: $unsupported")
    }
  }
}