package com.owlandrews.nescala

import java.io.{FileInputStream, IOException}
import java.util.zip.CRC32

import scala.util.{Failure, Try}

/**
  * Created by Hywel on 5/30/16.
  */
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

  private def rom = if (!path.endsWith(".nes")) Failure(new IllegalArgumentException("Invalid filename")) else Try(new FileInputStream(path))

  private val raw = rom.map { validFile =>
    Iterator.continually(validFile.read).takeWhile(x => x != -1).toArray
  }.getOrElse(throw new IOException(s"Unable to load rom file: $path"))

  lazy val Header = iNesHeader(raw.take(headerSize))

  lazy val CRC = new CRC32() {
    update(raw.map(_.toByte).takeRight(raw.length - headerSize))
  }.getValue.toHexString.toUpperCase

  val Mapper = (Header.Control2 & 0xF0) | (Header.Control1 >>> 4)

  val Mirror = (Header.Control1 & 1)  | (((Header.Control1 >>> 3) & 1) << 1)

  val Battery = (Header.Control1 >>> 1) & 1

  val HasTrainer = (Header.Control1 & 4) == 4

  private def getCurrentOffset(i:Int = 0) = {
    val base = if (HasTrainer) headerSize + trainerSize else headerSize
    base + i
  }

  val Trainer = if(HasTrainer) {
    raw.slice(headerSize, getCurrentOffset())
  } else Array.fill(trainerSize){0}

  private val prgOffset = Header.NumPRG * prgRomSize
  val PrgRom = raw.slice(getCurrentOffset(), getCurrentOffset(prgOffset))

  private val chrOffset = Header.NumCHR * chrRomSize
  val ChrRom = if (Header.NumCHR == 0) Array.fill(chrRomSize){0}
               else raw.slice(getCurrentOffset(prgOffset), getCurrentOffset(prgOffset + chrOffset))

  val SRam = Array.fill[Int](0x2000)(0)

  override def toString =
    s"""CRC: $CRC
       |Mapper: $Mapper
       |Mirror: $Mirror
       |Battery: $Battery""".stripMargin
}
