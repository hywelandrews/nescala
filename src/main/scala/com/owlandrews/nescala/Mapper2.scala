package com.owlandrews.nescala

case class Mapper2(mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {
  private val prgBanks = prgRom.length / 0x4000

  private var prgBank1 = 0
  private val prgBank2 = prgBanks - 1

  override def Read(address: Int): Int = address as uShort match {
    case chr if isChr(chr) => chrRom(address)
    case prg2 if isPrg2(prg2) =>
      val index = prgBank2 * 0x4000 + (prg2 - 0xC000)
      prgRom(index)
    case prg1 if isPrg1(prg1) =>
      val index = prgBank1 * 0x4000 + (prg1 - 0x8000)
      prgRom(index)
    case saveRam if isSRam(saveRam) =>
      val index = address - sRamAddress
      sRam(index)
    case default => System.err.println(s"Unhandled mapper2 read at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case chr if isChr(chr) => chrRom(address) = value
    case prg1 if isPrg1(prg1) => prgBank1 = value % prgBanks
    case saveRam if isSRam(saveRam) =>
      val index = address - sRamAddress
      sRam(index) = value
    case default => System.err.println(s"Unhandled mapper2 write at address: ${Integer.toHexString(default)}")
  }

  override def Mirror: Int = mirror
}