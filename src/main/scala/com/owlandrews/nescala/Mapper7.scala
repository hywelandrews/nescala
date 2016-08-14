package com.owlandrews.nescala

import com.owlandrews.nescala.helpers.Unsigned._

case class Mapper7(mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {
  private var prgBank = 0
  private var mirrorMode = mirror

  override def Read(address: Int): Int = address as uShort match {
    case chr if isChr(chr) => chrRom(address)
    case prg1 if isPrg1(prg1) =>
      val index = prgBank * 0x8000 + (prg1 - 0x8000)
      prgRom(index)
    case saveRam if isSRam(saveRam) =>
      val index = address - sRamAddress
      sRam(index)
    case default => System.err.println(s"Unhandled mapper7 read at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case chr if isChr(chr) => chrRom(chr) = value
    case prg1 if isPrg1(prg1) =>
      prgBank = value & 7
      value & 0x10 match {
        case 0x00 => mirrorMode = MirrorMode.Single0
        case 0x10 => mirrorMode = MirrorMode.Single1
      }
    case saveRam if isSRam(saveRam) =>
      val index = saveRam - sRamAddress
      sRam(index) = value
    case default => System.err.println(s"Unhandled mapper7 write at address: ${Integer.toHexString(default)}")
  }

  override def Mirror: Int = mirrorMode
}