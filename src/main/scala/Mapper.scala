trait Mapper {
  def Read(address:Int): Int
  def Write(address:Int, value: Int)
  def Step()
  val sRamAddress = 0x6000
  val isChr: Int => Boolean = x => x < 0x2000
  val isPrg1: Int => Boolean = x => x >= 0x8000
  val isSRam: Int => Boolean = x => x >= sRamAddress
}

object Mirror {
  val Horizontal = 0
  val Vertical   = 1
  val Single0    = 2
  val Single1    = 3
  val Four       = 4
}

object Mapper {
  def apply(cartridge:Cartridge): Mapper = {
    cartridge.Mapper match {
      case 0 => Mapper2(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case 1 => Mapper1(cartridge.Mirror, cartridge.ChrRom, cartridge.PrgRom, cartridge.SRam)
      case unsupported => System.err.println(s"Unhandled mapper: $unsupported"); throw new Exception()
    }
  }
}

case class Mapper1(var mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {
  private var shiftRegister: Int = resetShiftRegister
  private var prgMode, chrMode, prgBank, chrBank0, chrBank1, control:Int = 0
  private val prgOffsets, chrOffsets:Array[Int] = Array[Int](2)

  def Read(address: Int): Int = address match {
      case chr if isChr(address) =>
        val bank = chr / 0x1000
        val offset = chr % 0x1000
        chrRom(chrOffsets(bank)+offset)
      case prg if isPrg1(address) => 0
      case saveRam if isSRam(address) => 0
      case default => System.err.println(s"Unhandled mapper1 read at address: ${Integer.toHexString(default)}"); throw new IndexOutOfBoundsException
    }

  override def Step(): Unit = ()

  override def Write(address: Int, value: Int): Unit = address match {
    case chr if isChr(address)  =>
      val bank = chr / 0x1000
      val offset = chr % 0x1000
      chrRom(this.chrOffsets(bank)+offset) = value
    case prg if isPrg1(address) => loadRegister(prg, value)
    case saveRam if isSRam(address) => sRam(saveRam-sRamAddress) = value
    case default => System.err.println(s"Unhandled mapper1 write at address: ${Integer.toHexString(default)}")
  }

  private def resetShiftRegister = 0x10.toByte

  private def loadRegister(address:Int, value:Int) {
    if ((value & 0x80) == 0x80) {
      shiftRegister = resetShiftRegister
      writeControl(control | 0x0C)
    } else {
      val complete = (shiftRegister & 1) == 1
      shiftRegister = shiftRegister >>> 1
      shiftRegister = shiftRegister | ((value & 1) << 4)
      if (complete) {
        writeRegister(address, shiftRegister)
        shiftRegister = resetShiftRegister
      }
    }
  }

  private def writeRegister(address:Int, value:Int) {
    address match {
      case controlSpace if address <= 0x9FFF => writeControl(value)
      case chr0Space    if address <= 0xBFFF => writeCHRBank0(value)
      case chr1Space    if address <= 0xDFFF => writeCHRBank1(value)
      case prgSpace     if address <= 0xFFFF => writePRGBank(value)
    }
  }

  // Control (internal, $8000-$9FFF)
  private def writeControl(value:Int) {
    control = value
    chrMode = (value >> 4) & 1
    prgMode = (value >> 2) & 3
    value & 3 match {
      case 0 => mirror = Mirror.Single0
      case 1 => mirror = Mirror.Single1
      case 2 => mirror = Mirror.Vertical
      case 3 => mirror = Mirror.Horizontal
    }
    updateOffsets()
  }

  // CHR bank 0 (internal, $A000-$BFFF)
  private val writeCHRBank0:Int => Unit = x => chrBank0 = x; updateOffsets()

  // CHR bank 1 (internal, $C000-$DFFF)
  private val writeCHRBank1:Int => Unit = x => chrBank1 = x; updateOffsets()

  private val writePRGBank:Int => Unit = x => prgBank = (x & 0x0F).toByte; updateOffsets()

  private def chrBankOffset(index: Int): Int = {
    if (index >= 0x80) return 0x100.toByte
    val bankLength = chrRom.length

    val offset = findOffset(index, bankLength, 0x1000)

    if (offset < 0) return (offset + bankLength).toByte

    offset.toByte
  }
  private def prgBankOffset(index:Int): Int = {
    if (index >= 0x80) return 0x100

    val bankLength = prgRom.length
    val offset = findOffset(index, bankLength, 0x4000)

    if (offset < 0) return offset + bankLength

    offset
  }

  private def findOffset(index:Int, bankLength:Int, location:Int) = (index % bankLength / location) * location

  // PRG ROM bank mode (0, 1: switch 32 KB at $8000, ignoring low bit of bank number;
  //                    2: fix first bank at $8000 and switch 16 KB bank at $C000;
  //                    3: fix last bank at $C000 and switch 16 KB bank at $8000)
  // CHR ROM bank mode (0: switch 8 KB at a time; 1: switch two separate 4 KB banks)
  private def updateOffsets() = prgMode match {
      case 0 | 1 =>
        prgOffsets(0) = prgBankOffset(prgMode & 0xFE)
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
}

case class Mapper2(var mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {
  var prgBanks = prgRom.length / 0x4000
  var prgBank1 = 0
  var prgBank2 = prgBanks - 1
  val isPrg2:Int => Boolean = x => x >= 0xC000

  override def Read(address: Int): Int = address match {
    case chr if isChr(address) => chrRom(address)
    case prg2 if isPrg2(address) =>
      val index = prgBank2 * 0x4000 + (prg2 - 0xC000)
      prgRom(index)
    case prg1 if isPrg1(address) =>
      val index = prgBank1 * 0x4000 + (prg1 - 0x8000)
      prgRom(index)
    case saveRam if isSRam(address) =>
      val index = address - sRamAddress
      sRam(index)
    case default => System.err.println(s"Unhandled mapper2 read at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Step(): Unit = ()

  override def Write(address: Int, value: Int): Unit = address match {
    case chr if isChr(address) => chrRom(address) = value
    case prg1 if isPrg1(address) => prgBank1 = value % prgBanks
    case saveRam if isSRam(address) =>
      val index = address - sRamAddress
      sRam(index) = value
    case default => System.err.println(s"Unhandled mapper2 write at address: ${Integer.toHexString(default)}")
  }
}

