package com.owlandrews.nescala

import helpers.Unsigned._

case class Mapper1(mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {
  private val resetShiftRegister = 0x10
  private val prgOffsets = Array(0, prgBankOffset(-1))
  private val chrOffsets = Array(0, 0)

  private val suRom = chrRom.length == 0x2000 && prgRom.length == 0x80000

  private var suRomLatch = false
  private var shiftRegister = resetShiftRegister
  private var mirrorMode = mirror
  private var prgMode, chrMode, prgBank, chrBank0, chrBank1, control: Int = 0

  def Read(address: Int): Int = address as uShort match {
    case chr if isChr(chr) =>
      val bank = chr / 0x1000
      val offset = chr % 0x1000
      chrRom(chrOffsets(bank) + offset)
    case prg if isPrg1(prg) =>
      val loadAddress = (address - 0x8000) as uShort
      val bank = loadAddress / 0x4000
      val offset = loadAddress % 0x4000
      prgRom(prgOffsets(bank) + offset)
    case saveRam if isSRam(saveRam) => sRam(saveRam - sRamAddress)
    case default => throw new IndexOutOfBoundsException(s"Unhandled mapper1 read at address: ${Integer.toHexString(default)}")
  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case chr if isChr(address) =>
      val bank = chr / 0x1000
      val offset = chr % 0x1000
      chrRom(chrOffsets(bank) + offset) = value
    case prg if isPrg1(address) => loadRegister(prg, value)
    case saveRam if isSRam(address) => sRam(saveRam - sRamAddress) = value
    case default => throw new IndexOutOfBoundsException(s"Unhandled mapper1 write at address: ${Integer.toHexString(default)}")
  }

  override def Mirror: Int = mirrorMode

  private def loadRegister(address: Int, value: Int) = {
    if ((value & 0x80) == 0x80) {
      shiftRegister = resetShiftRegister
      writeControl(control | 0x0C)
    } else {
      val complete = (shiftRegister & 1) == 1
      shiftRegister = shiftRegister >>> 1
      shiftRegister = (shiftRegister | ((value & 1) << 4)) as uByte
      if (complete) {
        writeRegister(address, shiftRegister)
        shiftRegister = resetShiftRegister
      }
    }
  }

  private def writeRegister(address: Int, value: Int) = address match {
    case controlSpace if address <= 0x9FFF => writeControl(value)
    case chr0Space    if address <= 0xBFFF => writeCHRBank0(value)
    case chr1Space    if address <= 0xDFFF => writeCHRBank1(value)
    case prgSpace     if address <= 0xFFFF => writePRGBank(value)
  }

  // Control (internal, $8000-$9FFF)
  private def writeControl(value: Int) = {
    control = value
    chrMode = (value >>> 4) & 1
    prgMode = (value >>> 2) & 3
    mirrorMode = value & 3 match {
      case 0 => MirrorMode.Single0
      case 1 => MirrorMode.Single1
      case 2 => MirrorMode.Vertical
      case 3 => MirrorMode.Horizontal
    }
    updateOffsets()
  }

  // CHR bank 0 (internal, $A000-$BFFF)
  private def writeCHRBank0(x: Int):Unit = {
    if(suRom){
      chrBank0 = x & 0xF
      suRomLatch = ((x & 0x10) >> 4) == 1
    } else chrBank0 = x & 0x1F

    updateOffsets()
  }

  // CHR bank 1 (internal, $C000-$DFFF)
  private def writeCHRBank1(x: Int):Unit = {
    chrBank1 = if(suRom) x & 0xF else x & 0x1F
    updateOffsets()
  }

  private def writePRGBank(x: Int):Unit = {
    prgBank = x & 0x0F
    updateOffsets()
  }

  private def chrBankOffset(index: Int): Int = {
    val bankLength = chrRom.length
    val y = if (index >= 0x80) index - 0x100 else index
    val offset = findOffset(y, bankLength, 0x1000)

    if (offset < 0) offset + bankLength
    else offset
  }

  private def prgBankOffset(index: Int): Int = {
    val bankLength = if (suRom) prgRom.length / 2 else prgRom.length
    val y = if (index >= 0x80) index - 0x100 else index

    val offset = findOffset(y, bankLength, 0x4000)

    if (offset < 0) offset + bankLength else offset
  }

  private def findOffset(index: Int, bankLength: Int, size: Int) = (index % (bankLength / size)) * size

  // PRG ROM bank mode (0, 1: switch 32 KB at $8000, ignoring low bit of bank number;
  //                    2: fix first bank at $8000 and switch 16 KB bank at $C000;
  //                    3: fix last bank at $C000 and switch 16 KB bank at $8000)
  // CHR ROM bank mode (0: switch 8 KB at a time; 1: switch two separate 4 KB banks)
  private def updateOffsets() = {
    prgMode match {
      case 0 | 1 =>
        prgOffsets(0) = prgBankOffset(prgBank & 0xFE)
        prgOffsets(1) = prgBankOffset(prgBank | 0x01)
      case 2 =>
        prgOffsets(0) = 0
        prgOffsets(1) = prgBankOffset(prgBank)
      case 3 =>
        prgOffsets(0) = prgBankOffset(prgBank)
        prgOffsets(1) = prgBankOffset(-1)
    }

    chrMode match {
      case 0 =>
        chrOffsets(0) = chrBankOffset(chrBank0 & 0xFE)
        chrOffsets(1) = chrBankOffset(chrBank0 | 0x01)
      case 1 =>
        chrOffsets(0) = chrBankOffset(chrBank0)
        chrOffsets(1) = chrBankOffset(chrBank1)
    }

    if(suRom && suRomLatch) {
      prgOffsets(0) = prgOffsets(0) + 0x40000
      prgOffsets(1) = prgOffsets(1) + 0x40000
    }
  }
}