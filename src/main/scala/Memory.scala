/**
 * Created by Hywel on 4/14/15.
 */
trait Memory {
    def Read(address:Int):Int
    def Write(address:Int, value:Int)
}

case class CPUMemory(ram:Array[Int], ppu:PPU, apu:APU, controller1:Controller, controller2:Controller, mapper:Mapper) extends Memory {
  private val isRam:  Int => Boolean = x => x < 0x2000
  private val isPpu1: Int => Boolean = x => x < 0x4000
  private val isPpu2: Int => Boolean = x => x == 0x4014
  private val isApu1: Int => Boolean = x => x < 0x4014
  private val isApu2: Int => Boolean = x => x == 0x4015
  private val isController1: Int => Boolean = x => x == 0x4016
  private val isMapper: Int => Boolean = x => x >= 0x6000

  override def Read(address: Int): Int = address match {
    case ramAddress if isRam(address)   => ram(ramAddress % 0x0800)
    case ppuAddress if isPpu1(address)  => ppu.ReadRegister(0x2000 + ppuAddress % 8)
    case ppuAddress if isPpu2(address)  => ppu.ReadRegister(ppuAddress)
    case apuAddress if isApu2(address)  => apu.ReadRegister(address)
    case controller1Address if isController1(address) => controller1.Read()
    case controller2Address if address == 0x4017 => controller2.Read()
    //case ioAddress if address < 0x6000 => // TODO: I/O registers
    case mapperAddress if isMapper(address) => mapper.Read(address)
    case default => System.err.println(s"Unhandled cpu memory read at address: ${Integer.toHexString(default)}"); 0
  }

  override def Write(address: Int, value: Int): Unit = address match {
      case ramAddress if isRam(address) => ram(ramAddress % 0x0800) = value
      case ppuAddress if isPpu1(address) => ppu.WriteRegister(0x2000 + ppuAddress % 8, value, this.Read)
      case apuAddress if isApu1(address) => apu.WriteRegister(apuAddress, value)
      case ppuAddress if isPpu2(address) => ppu.WriteRegister(ppuAddress, value, this.Read)
      case apuAddress if isApu2(address)  => apu.WriteRegister(apuAddress, value)
      case controller1Address if isController1(address)  => controller1.Write(value)
                                                            controller2.Write(value)
      case apuAddress if address == 0x4017 => apu.WriteRegister(address, value)
      //case ioAddress if address < 0x6000 => // TODO: I/O registers
      case mapperAddress if isMapper(address)  => mapper.Write(address, value)
      case default => System.err.println(s"Unhandled cpu memory write at address: ${Integer.toHexString(default)}")
    }
}

case class PPUMemory(cartridge:Cartridge, mapper:Mapper) extends Memory {
  private val nameTableData = new Array[Int](2048)
  private val paletteData   = new Array[Int](32)

  override def Read(address: Int): Int = address % 0x4000 match {
    case mapperAddress if address < 0x2000 => mapper.Read(mapperAddress)
    case ppuNameTable if address < 0x3F00 => nameTableData(mirrorAddress(cartridge.Mirror, ppuNameTable) % 2048)
    case ppuPalette if address < 0x4000 => ReadPalette(ppuPalette % 32)
    case default => System.err.println(s"unhandled ppu memory read at address: ${Integer.toHexString(default)}"); 0
  }

  override def Write(address: Int, value: Int) = address % 0x4000 match {
      case mapperAddress if address < 0x2000 => mapper.Write(address, value)
      case ppuNameTable if address < 0x3F00 => nameTableData(mirrorAddress(cartridge.Mirror, address) % 2048) = value
      case ppuPalette if address < 0x4000 => WritePalette(address % 32, value)
      case default => System.err.println(s"unhandled ppu memory write at address: ${Integer.toHexString(default)}")
  }

  private val mirrorLookup = Array(
    Array(0, 0, 1, 1),
    Array(0, 1, 0, 1),
    Array(0, 0, 0, 0),
    Array(1, 1, 1, 1),
    Array(0, 1, 2, 3)
  )

  private def mirrorAddress(mode:Int, address:Int):Int = {
    val mirrorAddress = (address - 0x2000) % 0x1000
    val table = mirrorAddress / 0x0400
    val offset = mirrorAddress % 0x0400
    0x2000 + mirrorLookup(mode)(table) * 0x0400 + offset
  }

  def ReadPalette(address:Int):Int = {
    if (!(address >= 16) || !(address % 4 == 0)) paletteData(address)
    else paletteData(address - 16)
  }

  def WritePalette(address:Int, value:Int) = {
    if (!(address >= 16) || !(address % 4 == 0)) paletteData(address) = value
    else paletteData(address - 16) = value
  }
}
