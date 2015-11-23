package nescala

import java.io.FileInputStream

import scala.util.{Failure, Try}
/**
 * Created by Hywel on 4/14/15.
 */
final case class Cartridge(private val path:String) {

  private val headerSize = 16
  private val trainerSize = 512
  private val prgRomSize = 16384
  private val chrRomSize = 8192

  case class iNesHeader(file:Array[Int]) {
    private val fileMagic = 0x4E45531A
    val Magic:Int = Integer.parseInt(file.take(4).map(Integer.toHexString).reduce(_ + _), 16)
    val NumPRG:Int = file(4)
    val NumCHR:Int = file(5)
    val Control1:Int = file(6)
    val Control2:Int = file(7)
    val NumRam:Int = file(8)
    val Padding:Array[Int] = file.slice(9, 15)

    if (Magic != fileMagic) throw new IllegalArgumentException("Invalid iNes file")

    override def toString =
      s"""Header NumPRG: $NumPRG
         |Header NumCHR: $NumCHR
         |Header Control1: $Control1
         |Header Control2: $Control2
         |Header NumRam: $NumRam
       """.stripMargin
  }

  private val rom = if (!path.endsWith(".nes")) Failure(new IllegalArgumentException("Invalid filename")) else Try(new FileInputStream(path))

  private val byteArray = rom.map { validFile =>
    Stream.continually(validFile.read)
    .takeWhile(x => x != -1)
    .toArray
  }.getOrElse(throw new RuntimeException(s"Unable to load rom file: $path"))

  val Header = iNesHeader(byteArray.take(headerSize))

  val Mapper = Header.Control1 >> 4 | Header.Control2 >>> 4 << 1

  val Mirror = Header.Control1 & 1  | (Header.Control1 >>> 3 & 1) << 1

  val Battery = Header.Control1 >>> 1 & 1

  val HasTrainer = (Header.Control1 & 4) == 4

  private var offset = headerSize

  val Trainer = if(HasTrainer) {
    offset += trainerSize
    byteArray.slice(offset, offset + trainerSize)
  } else Array.fill(trainerSize){0}

  val PrgRom = byteArray.slice(offset, offset + Header.NumPRG * prgRomSize)

  offset += (Header.NumPRG * prgRomSize)

  val ChrRom = if (Header.NumCHR == 0) Array.fill(chrRomSize){0}
               else byteArray.slice(offset, offset + Header.NumCHR * chrRomSize)

  val SRam = new Array[Int](0x2000)

  override def toString =
    s"""Mapper: $Mapper
       |Mirror: $Mirror
       |Battery: $Battery
    """.stripMargin
}
