package com.owlandrews.nescala

import com.owlandrews.nescala.helpers.Unsigned._

case class Mapper5(mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {

  var prgMode           = 3
  var chrMode           = 0
  var prgRamProtect1    = 0
  var prgRamProtect2    = 0
  var extendedRamMode   = 0
  var prgRam            = Array.fill[Int](65536)(0)
  var prgOffsets        = Array.fill[Array[Int]](4)(Array.fill[Int](4)(0))
  var chrOffsets        = Array.fill[Array[Int]](12)(Array.fill[Int](4)(0))
  var extendedRam       = Array.fill[Int](1024)(0)
  var prgRamOffset      = 0
  var nameTableMapping  = 0
  var fillModeTile      = 0
  var fillModeColour    = 0
  var spriteSize        = 0

  override def Read(address: Int): Int = address as uShort match {
    case chr  if isChr(address) => println(s"read chr $address");readChr(address)
    case prg1 if isPrg1(address) => readPrg(address)
    case saveRam if isSRam(address)  =>
      val offset = address - 0x6000
      prgRam(offset + prgRamOffset)
    case default => System.err.println(s"Unhandled mapper5 read at address: ${Integer.toHexString(default)}"); 0

  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case chr  if isChr(address) => println(s"write chr $address");writeChr(address, value)
    case prg1 if isPrg1(address) => writePrg(address, value)
    case saveRam if isSRam(address)  =>
      val offset = address - 0x6000
      prgRam(offset + prgRamOffset) = value
    case control if address >= 0x5100 => writeRegister(control, value)
    case default => System.err.println(s"Unhandled mapper5 write at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Mirror = mirror

  override def Step(ppuCycle:Long, ppuScanLine:Long, ppuFlagShowBackground:Int, ppuFlagShowSprites:Int, flagSpriteSize:Int, triggerIRQHandler: => Unit) : Unit = {
    spriteSize = flagSpriteSize
  }

  private def readChr(address:Int) = chrMode match {
    case 0 =>
      val bank = if (spriteSize == 1) chrOffsets(11)(3) else chrOffsets(7)(3)
      val index = (bank * 0x2000) + address
      chrRom(index)
    case 1 =>
      val bank = address match {
        case chr1 if address < 0x0FFF => chrOffsets(3)(2)
        case chr1 if address < 0x1FFF => if (spriteSize == 1) chrOffsets(11)(2) else chrOffsets(7)(2)
      }
      val index = (bank * 0x1000) + address
      chrRom(index)
    case 2 =>
      val bank = address match {
        case chr1 if address < 0x07FF => if (spriteSize == 1) chrOffsets(9)(2) else chrOffsets(1)(1)
        case chr1 if address < 0x0FFF => if (spriteSize == 1) chrOffsets(11)(2) else chrOffsets(3)(1)
        case chr1 if address < 0x17FF => if (spriteSize == 1) chrOffsets(9)(2) else chrOffsets(5)(1)
        case chr1 if address < 0x1FFF => if (spriteSize == 1) chrOffsets(11)(2) else chrOffsets(7)(2)
      }
      val index = (bank * 0x0800) + address
      chrRom(index)
    case 3 => 0x0400
  }

  private def writeChr(address:Int, value:Int) = chrMode match {
    case 0 =>
      val bank = if (spriteSize == 1) chrOffsets(11)(3) else chrOffsets(7)(3)
      val index = (bank * 0x2000) + address
      chrRom(index) = value
    case 1 =>
      val bank = address match {
        case chr1 if address < 0x0FFF => if (spriteSize == 1) chrOffsets(11)(2) else chrOffsets(3)(2)
        case chr1 if address < 0x1FFF => if (spriteSize == 1) chrOffsets(11)(2) else chrOffsets(7)(2)
      }
      val index = (bank * 0x1000) + address
      chrRom(index) = value
    case 2 =>
      val bank = address match {
        case chr1 if address < 0x07FF => if (spriteSize == 1) chrOffsets(9)(2) else chrOffsets(1)(1)
        case chr1 if address < 0x0FFF => if (spriteSize == 1) chrOffsets(11)(2) else chrOffsets(3)(1)
        case chr1 if address < 0x17FF => if (spriteSize == 1) chrOffsets(9)(2) else chrOffsets(5)(1)
        case chr1 if address < 0x1FFF => if (spriteSize == 1) chrOffsets(11)(2) else chrOffsets(7)(2)
      }
      val index = (bank * 0x0800) + address
      chrRom(index) = value
    case 3 => 0x0400
  }

  private def readPrg(address:Int) = address match {
    case bank0 if address < 0xA000 && prgMode == 3 =>
      val bank =  prgOffsets(0)(prgMode) & 0x7F
      val index = (bank * 0x2000) + (address - 0x8000)
      prgRom(index)
    case bank1 if address < 0xC000 =>
      val bank =  prgOffsets(1)(prgMode)
      val index = if(prgMode == 1 || prgMode == 2) {
        if (isPrgRam(bank)) ((bank & 0xF8) * 0x4000) + (address - 0xA000) else ((bank & 0x7E) * 0x4000) + (address - 0xA000)
      } else ((bank & 0x7E) * 0x2000) + (address - 0xA000)

      if (isPrgRam(bank)) prgRam(index)
      else prgRom(index)
    case bank3 if address < 0xE000 =>
      val bank = prgOffsets(2)(prgMode)
      val index = if (isPrgRam(bank)) ((bank & 0xF8) * 0x2000) + address - 0xC000 else ((bank & 0x7F) * 0x2000) + address - 0xC000
      if (isPrgRam(bank))  prgRam(index)
      else prgRom(index)
    case bank4 if address < 0x10000 =>
      val bank = prgOffsets(3)(prgMode) & 0x7F
      val index =  if(prgMode == 0) (bank * 0x8000) + address - 0xE000 else if(prgMode == 1) (bank * 0x4000) + address - 0xE000 else (bank * 0x2000) + address - 0xE000
      prgRom(index)
    case _ => 0
  }

  private def writePrg(address:Int, value:Int) = address match {
    case bank0 if address < 0xA000 && prgMode == 3 =>
      val bank =  prgOffsets(0)(prgMode) & 0x7F
      val index = (bank * 0x2000) + (address - 0x8000)
      prgRom(index) = value
    case bank1 if address < 0xC000 =>
      val bank =  prgOffsets(1)(prgMode)
      val index = if(prgMode == 1 || prgMode == 2) {
        if (isPrgRam(bank)) ((bank & 0xF8) * 0x4000) + (address - 0xA000) else ((bank & 0x7E) * 0x4000) + (address - 0xA000)
      } else ((bank & 0x7E) * 0x2000) + (address - 0xA000)

      if (isPrgRam(bank)) prgRam(index) = value
      else prgRom(index) = value
    case bank3 if address < 0xE000 =>
      val bank = prgOffsets(2)(prgMode)
      val index = if (isPrgRam(bank)) ((bank & 0xF8) * 0x2000) + address - 0xC000 else ((bank & 0x7F) * 0x2000) + address - 0xC000
      if (isPrgRam(bank))  prgRam(index) = value
      else prgRom(index) = value
    case bank4 if address < 0x10000 =>
      val bank = prgOffsets(3)(prgMode) & 0x7F
      val index =  if(prgMode == 0) (bank * 0x8000) + address - 0xE000 else if(prgMode == 1) (bank * 0x4000) + address - 0xE000 else (bank * 0x2000) + address - 0xE000
      prgRom(index) = value
    case _ =>
  }

  private def isPrgRam(value:Int) = (value & 0x80) == 0

  private def writeRegister(address:Int, value:Int) = address match {
    case 0x5100   => writePrgMode(value)
    case 0x5101   => writeChrMode(value)
    case 0x5102   => writeRamProtect1(value)
    case 0x5103   => writeRamProtect2(value)
    case 0x5104   => writeExtendedRamMode(value)
    case 0x5105   => nameTableMapping = value
    case 0x5106   => fillModeTile = value
    case 0x5107   => fillModeColour = value & 3
    case 0x5113   => writePrgRamBank(value)
    case 0x5114   => writePrgBank0(value)
    case 0x5115   => writePrgBank1(value)
    case 0x5116   => writePrgBank2(value)
    case 0x5117   => writePrgBank3(value)
    case chrBank  if address >= 0x5120 && address <= 0x512B => writeChrBank(address, value)
  }

  private def writePrgMode(value:Int) = {
    prgMode = value & 3
  }

  private def writeChrMode(value:Int) = {
    chrMode = value & 3
  }

  private def writeRamProtect1(value:Int) = {
    prgRamProtect1 = value & 3
  }

  private def writeRamProtect2(value:Int) = {
    prgRamProtect2 = value & 3
  }

  private def writeExtendedRamMode(value:Int) = {
    extendedRamMode = value & 3
  }

  private def writePrgRamBank(value:Int) = {
    prgRamOffset = value & 7
  }

  private def writePrgBank0(value:Int) = if(prgMode == 3) writePrgBank(value, prgMode, 0)

  private def writePrgBank1(value:Int) = prgMode match{
    case 1 => writePrgBank(value & 0x1, prgMode, 1)
    case 2 => writePrgBank(value & 0x1, prgMode, 1)
    case 3 => writePrgBank(value, prgMode, 1)
    case _ =>
  }

  private def writePrgBank2(value:Int) = if(prgMode == 2 || prgMode == 3) writePrgBank((value | 0x80) & 0x1, prgMode, 2)

  private def writePrgBank3(value:Int) = prgMode match{
    case 2 => writePrgBank((value | 0x80) & 0x1, prgMode, 3)
    case 3 => writePrgBank((value | 0x80) & 0x3, prgMode, 3)
    case _ =>
  }

  private def writePrgBank(value:Int, prgMode:Int, index:Int) = {
    prgOffsets(index)(prgMode) = value
  }

  private def writeChrBank(address:Int, value:Int) = chrMode match {
    case 0 => address match {
      case 0x5127 => chrOffsets(7)(3) = value
      case 0x512B => chrOffsets(11)(3) = value
    }
    case 1 => address match {
      case 0x5123 => chrOffsets(3)(2) = value
      case 0x5127 => chrOffsets(7)(2) = value
      case 0x512B => chrOffsets(11)(2) = value
    }
    case 2 => address match {
      case 0x5121 => chrOffsets(1)(1) = value
      case 0x5123 => chrOffsets(3)(1) = value
      case 0x5125 => chrOffsets(5)(1) = value
      case 0x5127 => chrOffsets(7)(1) = value
      case 0x5129 => chrOffsets(9)(1) = value
      case 0x512B => chrOffsets(11)(1) = value
    }
    case 3 => address match {
      case 0x5120 => chrOffsets(0)(0) = value
      case 0x5121 => chrOffsets(1)(0) = value
      case 0x5122 => chrOffsets(2)(0) = value
      case 0x5123 => chrOffsets(3)(0) = value
      case 0x5124 => chrOffsets(4)(0) = value
      case 0x5125 => chrOffsets(5)(0) = value
      case 0x5126 => chrOffsets(6)(0) = value
      case 0x5127 => chrOffsets(7)(0) = value
      case 0x5128 => chrOffsets(8)(0) = value
      case 0x5129 => chrOffsets(9)(0) = value
      case 0x512A => chrOffsets(10)(0) = value
      case 0x512B => chrOffsets(11)(0) = value
    }
  }
}

