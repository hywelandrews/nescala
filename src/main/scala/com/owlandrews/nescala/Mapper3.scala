package com.owlandrews.nescala

case class Mapper3(var mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {

  var prgBanks = prgRom.length / 0x4000
  var chrBank  = 0
  var prgBank1 = 0
  var prgBank2 = prgBanks - 1

  override def Read(address:Int): Int =  address as uShort match {
    case chr if isChr(address) =>
      val index = chrBank * 0x2000 + address
      chrRom(index)
    case prg2 if isPrg2(address) =>
      val index = prgBank2 * 0x4000 + (address - 0xC000)
      prgRom(index)
    case prg1 if isPrg1(address) =>
      val index = prgBank1 * 0x4000 + (address - 0x8000)
      prgRom(index)
    case saveRam if address >= 0x6000 =>
      val index = (address - 0x6000) as uShort
      sRam(index)
    case default => System.err.println(s"Unhandled mapper3 write at address: ${Integer.toHexString(default)}"); 0
  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case chr if isChr(address) =>
      val index = chrBank * 0x2000 + address
      chrRom(index) = value
    case prg1 if isPrg1(address) => chrBank = value & 3
    case saveRam if isSRam(address) =>
      val index = address - 0x6000
      sRam(index) = value
    case default => System.err.println(s"Unhandled mapper3 read at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Mirror: Int = mirror
}