package com.owlandrews.nescala

import com.owlandrews.nescala.helpers.Unsigned._

case class Mapper23(mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {
  private val prgBanks = prgRom.length / 0x2000

  private var prgBank1 = 0
  private var prgBank2 = 0
  private val prgBank3 = prgBanks - 2
  private var microWireLatch = 0
  private val chrOffsets = Array.fill[Int](8)(0)

  val isMicroWire: Int => Boolean = x => x >= 0x6000

  private var mirrorMode = mirror

  override def Read(address: Int): Int = address as uShort match {
    case chr if isChr(chr) =>
      val bank = address / 0x0400
      val offset = address % 0x0400
      chrRom((chrOffsets(bank) * 0x0400) + offset)
    case prg3 if prg3 >= 0xC000 =>
      val index = prgBank3 * 0x2000 + (prg3 - 0xC000)
      prgRom(index)
    case prg2 if prg2 >= 0xA000 =>
      val index = prgBank2 * 0x2000 + (prg2 - 0xA000)
      prgRom(index)
    case prg1 if isPrg1(prg1) =>
      val index = prgBank1 * 0x2000 + (prg1 - 0x8000)
      prgRom(index)
    case microWire if isMicroWire(microWire) =>
      val openBus = microWire - 0x6FFF > 0
      if(openBus) 0 else microWireLatch
    case default => System.err.println(s"Unhandled mapper22 read at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case chr if isChr(chr) =>
      val bank = address / 0x0400
      val offset = address % 0x0400
      chrRom((chrOffsets(bank) * 0x0400) + offset) = value
    case prg1 if isPrg1(prg1) => writeRegister(address, value)
    case microWire if isMicroWire(microWire) => microWireLatch = value & 0x1
    case default => System.err.println(s"Unhandled mapper22 write at address: ${Integer.toHexString(default)}")
  }

  private def writeRegister(address:Int, value:Int) = address match {
    case chr        if address >= 0xB000 => writeChrBank(address, value)
    case prg2       if address >= 0xA000 => prgBank2 = value & 0xF
    case setMirror  if address >= 0x9000 => writeMirror(value)
    case prg1       if address >= 0x8000 => prgBank1 = value & 0xF
  }

  private def writeChrBank(address:Int, value:Int) = {
    val index = (((address % 0xB000) / 0x1000) * 2) + ((address % 0x1000) / 2)

   chrOffsets(index) = {
     val isLowByte = address % 2 == 0
     val highBankValue =  chrOffsets(index) & 0xF0
     val lowBankValue  =  chrOffsets(index) & 0xF
     if(isLowByte) highBankValue | (value & 0xF) else ((value & 0xF) << 4) | lowBankValue
   }
  }

  private def writeMirror(value:Int) = value & 1 match {
    case 0 => mirrorMode = MirrorMode.Vertical
    case 1 => mirrorMode = MirrorMode.Horizontal
  }

  override def Mirror: Int = mirrorMode
}

