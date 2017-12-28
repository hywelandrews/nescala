package com.owlandrews.nescala

case class Mapper9(mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {

  private val prgBanks = prgRom.length / 0x2000

  private val chrOffsets = Array.fill[Int](4)(0)
  private var prgBank1 = 0
  private val prgBank2 = prgBanks - 3
  private var latch0, latch1    = 0xFD
  private var mirrorMode = mirror

  override def Read(address: Int): Int = address as uShort match {
    case chr1 if address < 0x1000 =>
      val bank = if(latch0 == 0xFD) chrOffsets(0) else chrOffsets(1)
      setLatch(chr1)
      val index = (bank * 0x1000) + chr1
      chrRom(index)
    case chr2 if isChr(address) =>
      val bank = if(latch1 == 0xFD) chrOffsets(2) else chrOffsets(3)
      setLatch(chr2)
      val index = (bank * 0x1000) + chr2 - 0x1000
      chrRom(index)
    case prg2 if address >= 0xA000 =>
      val index = prgBank2 * 0x2000 + (prg2 - 0xA000)
      prgRom(index)
    case prg1 if isPrg1(address) =>
      val index = prgBank1 * 0x2000 + (prg1 - 0x8000)
      prgRom(index)
    case saveRam if isSRam(address) =>
      val index = address - sRamAddress
      sRam(index)
    case default => System.err.println(s"Unhandled mapper9 read at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case mirror1 if address >= 0xF000 => value & 0x1 match {
      case 0 => mirrorMode = MirrorMode.Vertical
      case 1 => mirrorMode = MirrorMode.Horizontal
    }
    case chr1   if address >= 0xE000 => chrOffsets(3) = value & 0x1F
    case chr1   if address >= 0xD000 => chrOffsets(2) = value & 0x1F
    case chr2   if address >= 0xC000 => chrOffsets(1) = value & 0x1F
    case chr2   if address >= 0xB000 => chrOffsets(0) = value & 0x1F
    case prg1   if address >= 0xA000 => prgBank1 = value & 0xF
    case saveRam if isSRam(address) =>
      val index = address - sRamAddress
      sRam(index) = value
    case default => System.err.println(s"Unhandled mapper9 write at address: ${Integer.toHexString(default)}")
  }

  override def Mirror: Int = mirrorMode

  private def setLatch(address: Int): Unit = address match {
    case bank2 if bank2 >= 0x1FE8 && bank2 <= 0x1FEF => latch1 = 0xFE
    case bank2 if bank2 >= 0x1FD8 && bank2 <= 0x1FDF => latch1 = 0xFD
    case bank1 if bank1 == 0x0FD8 => latch0 = 0xFD
    case bank1 if bank1 == 0x0FE8 => latch0 = 0xFE
    case default =>
  }
}