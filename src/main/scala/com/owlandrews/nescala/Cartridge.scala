package com.owlandrews.nescala

import java.io.IOException
import java.nio.file.{Files, Paths}
import java.util.zip.CRC32

import com.owlandrews.nescala.helpers.Unsigned._

import scala.util.{Failure, Try}

final case class Cartridge(private val path:String) {

  private val headerSize  = 16
  private val trainerSize = 512
  private val prgRomSize  = 16384
  private val chrRomSize  = 8192

  case class iNesHeader(file:Array[Int]) {
    private val fileMagic = 0x4E45531A
    val Magic     = Integer.parseInt(file.take(4).map(Integer.toHexString).reduce(_ + _), 16)
    val NumPRG    = file(4)
    val NumCHR    = file(5)
    val Control1  = file(6)
    val Control2  = file(7)
    val NumRam    = file(8)
    val Padding   = file.slice(9, 15)

    if (Magic != fileMagic) throw new IllegalArgumentException("Invalid iNes file")

    override def toString = s"PRG: $NumPRG CHR: $NumCHR Ram: $NumRam"
  }

  private def load = if (!path.endsWith(".nes")) Failure(new IllegalArgumentException("Invalid filename")) else Try(Files.readAllBytes(Paths.get(path)))

  private val rom = load.map(file => file.map(_ as uByte)).getOrElse(throw new IOException(s"Unable to load rom file: $path"))

  lazy val Header = iNesHeader(rom.take(headerSize))

  lazy val CRC = new CRC32() {
    update(rom.map(_.toByte).takeRight(rom.length - headerSize))
  }.getValue.toHexString.toUpperCase

  lazy val Mapper = (Header.Control2 & 0xF0) | (Header.Control1 >>> 4)

  lazy val Mirror = (Header.Control1 & 1)  | (((Header.Control1 >>> 3) & 1) << 1)

  lazy val Battery = (Header.Control1 >>> 1) & 1

  lazy val HasTrainer = (Header.Control1 & 4) == 4

  private def getCurrentOffset(i:Int = 0) = {
    val base = if (HasTrainer) headerSize + trainerSize else headerSize
    base + i
  }

  lazy val Trainer = if(HasTrainer) {
    rom.slice(headerSize, getCurrentOffset())
  } else Array.fill(trainerSize){0}

  private val prgOffset = Header.NumPRG * prgRomSize

  lazy val PrgRom = rom.slice(getCurrentOffset(), getCurrentOffset(prgOffset))

  private val chrOffset = Header.NumCHR * chrRomSize

  lazy val ChrRom = if (Header.NumCHR == 0) Array.fill(chrRomSize){0}
               else rom.slice(getCurrentOffset(prgOffset), getCurrentOffset(prgOffset + chrOffset))

  lazy val SRam = Array.fill[Int](0x2000)(0)

  override def toString =
    s"""CRC: $CRC
       |Mapper: $Mapper
       |Mirror: $Mirror
       |Battery: $Battery""".stripMargin
}
