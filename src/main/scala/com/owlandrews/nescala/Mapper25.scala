package com.owlandrews.nescala

case class Mapper25(mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {
  private val prgBanks = prgRom.length / 0x2000

  private var prgBank1 = 0
  private var prgBank2 = 0
  private var prgBank3 = prgBanks - 2
  private val prgBank4 = prgBanks - 1
  private var prgSwapMode = 0
  private val chrOffsets = Array.fill[Int](8)(0)

  private var mirrorMode = mirror

  override def Read(address: Int): Int = address as uShort match {
    case chr if isChr(chr) =>
      val bank = address / 0x0400
      val offset = address % 0x0400
      chrRom((chrOffsets(bank) * 0x0400) + offset)
    case prg4 if prg4 >= 0xE000 =>
      val index = prgBank4 * 0x2000 + (prg4 - 0xE000)
      prgRom(index)
    case prg3 if prg3 >= 0xC000 =>
      val index = prgBank3 * 0x2000 + (prg3 - 0xC000)
      prgRom(index)
    case prg2 if prg2 >= 0xA000 =>
      val index = prgBank2 * 0x2000 + (prg2 - 0xA000)
      prgRom(index)
    case prg1 if isPrg1(prg1) =>
      val index = prgBank1 * 0x2000 + (prg1 - 0x8000)
      prgRom(index)
    case saveRam if isSRam(saveRam) =>
      val index = (address - 0x6000) as uShort
      sRam(index)
    case default => System.err.println(s"Unhandled mapper25 read at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case chr if isChr(chr) =>
      val bank = address / 0x0400
      val offset = address % 0x0400
      chrRom((chrOffsets(bank) * 0x0400) + offset) = value
    case prg1 if isPrg1(prg1) => writeRegister(address, value)
    case saveRam if isSRam(saveRam) =>
      val index = (address - 0x6000)  as uShort
      sRam(index) = value
    case default => System.err.println(s"Unhandled mapper25 write at address: ${Integer.toHexString(default)}")
  }

  private def writeRegister(address:Int, value:Int) = address match {
    case irq        if address >= 0xF000 =>
    case chr        if address >= 0xB000 => writeChrBank(address, value)
    case prg2       if address >= 0xA000 => prgBank2 = value & 0xF
    case setMirror  if address == 0x9000 || address == 0x9002 => writeMirror(value)
    case prgSwap    if address == 0x9001 || address == 0x9003 => prgSwapMode = (prgSwap & 0x2) >> 1
    case prg1       if address >= 0x8000 => writePrgBank(value & 0xF)
  }

  private def writeChrBank(address:Int, value:Int) = {
    val index = (((address % 0xB000) / 0x1000) * 2) + ((address % 0x1000) % 2)

   chrOffsets(index) = {
     val isLowByte = (address % 0x1000) <= 1
     val highBankValue =  chrOffsets(index) & 0x1F0
     val lowBankValue  =  chrOffsets(index) & 0xF
     if(isLowByte) highBankValue | (value & 0xF) else ((value & 0x1F) << 4) | lowBankValue
   }
  }

  private def writePrgBank(value:Int) = prgSwapMode match {
      case 0 =>
        prgBank1  = value
        prgBank3  = prgBanks - 2
      case 1 =>
        prgBank3 = value
        prgBank1 = prgBanks - 2
  }


  private def writeMirror(value:Int) = value & 3 match {
    case 0 => mirrorMode = MirrorMode.Vertical
    case 1 => mirrorMode = MirrorMode.Horizontal
    case 2 => mirrorMode = MirrorMode.Single0
    case 3 => mirrorMode = MirrorMode.Single1
  }

  override def Mirror: Int = mirrorMode
}

