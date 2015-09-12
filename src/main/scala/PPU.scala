import java.awt.image.BufferedImage

/**
 * Created by Hywel on 5/1/15.
 */
class PPU(val memory:PPUMemory) {

  var DMAStall = false

  private var cycle = 0
  // 0-340
  var scanLine = 0
  // 0-261, 0-239=visible, 240=post, 241-260=vblank, 261=pre
  private var frame = 0 // frame counter

  private val oamData = new Array[Int](256)
  private var front = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)
  private var back = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)
  // PPU registers
  // current vram address (15 bit) // temporary vram address (15 bit) // fine x scroll (3 bit) // write toggle (1 bit) // even/odd frame flag (1 bit)
  private var v, t, x, w, f = 0
  private var register = 0

  // NMI flags
  private var nmiOccurred, nmiOutput, nmiPrevious = false
  private var nmiDelay = 0

  // background temporary variables
  private var nameTableByte, attributeTableByte, lowTileByte, highTileByte, tileData = 0

  // sprite temporary variables
  private var spriteCount = 0
  private val spritePatterns, spritePositions, spritePriorities, spriteIndexes = new Array[Int](8)

  // $2000 PPUCTRL
  private var flagNameTable = 0
  // 0: $2000; 1: $2400; 2: $2800; 3: $2C00
  private var flagIncrement = 0
  // 0: add 1; 1: add 32
  private var flagSpriteTable = 0
  // 0: $0000; 1: $1000; ignored in 8x16 mode
  private var flagBackgroundTable = 0
  // 0: $0000; 1: $1000
  private var flagSpriteSize = 0
  // 0: 8x8; 1: 8x16
  private var flagMasterSlave = 0 // 0: read EXT; 1: write EXT

  // $2001 PPUMASK
  private var flagGrayscale = 0
  // 0: color; 1: grayscale
  private var flagShowLeftBackground = 0
  // 0: hide; 1: show
  private var flagShowLeftSprites = 0
  // 0: hide; 1: show
  private var flagShowBackground = 0
  // 0: hide; 1: show
  private var flagShowSprites = 0
  // 0: hide; 1: show
  private var flagRedTint = 0
  // 0: normal; 1: emphasized
  private var flagGreenTint = 0
  // 0: normal; 1: emphasized
  private var flagBlueTint = 0 // 0: normal; 1: emphasized

  // $2002 PPUSTATUS
  private var flagSpriteZeroHit, flagSpriteOverflow = 0

  // $2003 OAMADDR
  private var oamAddress = 0

  // $2007 PPUDATA
  private var bufferedData = 0 // for buffered reads

  def Reset() {
    this.cycle = 340
    this.scanLine = 240
    this.frame = 0
    writeControl(0)
    writeMask(0)
    writeOAMAddress(0)
  }

  // $2000: PPUCTRL
  private def writeControl(value: Int) = {
    this.flagNameTable = (value >> 0) & 3
    this.flagIncrement = (value >> 2) & 1
    this.flagSpriteTable = (value >> 3) & 1
    this.flagBackgroundTable = (value >> 4) & 1
    this.flagSpriteSize = (value >> 5) & 1
    this.flagMasterSlave = (value >> 6) & 1
    this.nmiOutput = ((value >> 7) & 1) == 1
    this.nmiChange()
    // t: ....BA.. ........ = d: ......BA
    this.t = (this.t & 0xF3FF) | (value & 0x03) << 10
  }

  // $2001: PPUMASK
  private def writeMask(value: Int) = {
    this.flagGrayscale = (value >> 0) & 1
    this.flagShowLeftBackground = (value >> 1) & 1
    this.flagShowLeftSprites = (value >> 2) & 1
    this.flagShowBackground = (value >> 3) & 1
    this.flagShowSprites = (value >> 4) & 1
    this.flagRedTint = (value >> 5) & 1
    this.flagGreenTint = (value >> 6) & 1
    this.flagBlueTint = (value >> 7) & 1
  }

  // $2002: PPUSTATUS
  private def readStatus(): Int = {
    var result = this.register & 0x1F
    result |= flagSpriteOverflow << 5
    result |= flagSpriteZeroHit << 6
    if (this.nmiOccurred) {
      result |= 1 << 7
    }
    this.nmiOccurred = false
    nmiChange()
    // w:                   = 0
    this.w = 0
    result
  }

  // $2003: OAMADDR
  private def writeOAMAddress(value: Int) = {
    this.oamAddress = value
  }

  // $2004: OAMDATA (read)
  private def readOAMData(): Int = {
    this.oamData(this.oamAddress)
  }

  // $2004: OAMDATA (write)
  private def writeOAMData(value: Int) = {
    this.oamData(oamAddress) = value
    this.oamAddress = this.oamAddress + this.oamAddress
  }

  // $2005: PPUSCROLL
  private def writeScroll(value: Int) = {
    if (this.w == 0) {
      // t: ........ ...HGFED = d: HGFED...
      // x:               CBA = d: .....CBA
      // w:                   = 1
      this.t = (this.t & 0xFFE0) | (value >>> 3)
      this.x = value & 0x07
      this.w = 1
    } else {
      // t: .CBA..HG FED..... = d: HGFEDCBA
      // w:                   = 0
      this.t = (this.t & 0x8FFF) | ((value & 0x07) << 12)
      this.t = (this.t & 0xFC1F) | ((value & 0xF8) << 2)
      this.w = 0
    }
  }

  // $2006: PPUADDR
  private def writeAddress(value: Int) = {
    if (this.w == 0) {
      // t: ..FEDCBA ........ = d: ..FEDCBA
      // t: .X...... ........ = 0
      // w:                   = 1
      this.t = (this.t & 0x80FF) | ((value & 0x3F) << 8)
      this.w = 1
    } else {
      // t: ........ HGFEDCBA = d: HGFEDCBA
      // v                    = t
      // w:                   = 0
      this.t = (this.t & 0xFF00) | value
      this.v = this.t
      this.w = 0
    }
  }

  // $2007: PPUDATA (read)
  private def readData(): Int = {
    var value = memory.Read(v)
    // emulate buffered reads
    if (this.v % 0x4000 < 0x3F00) {
      val buffered = this.bufferedData
      this.bufferedData = value
      value = buffered
    } else {
      this.bufferedData = memory.Read(this.v - 0x1000)
    }
    // increment address
    if (this.flagIncrement == 0) {
      this.v += 1
    } else {
      this.v += 32
    }
    value
  }

  // $2007: PPUDATA (write)
  private def writeData(value: Int) = {
    memory.Write(this.v, value)
    if (this.flagIncrement == 0) {
      this.v += 1
    } else {
      this.v += 32
    }
  }

  // $4014: OAMDMA
  private def writeDMA(value: Int, dmaRead: Int => Int) = {
    var address = value << 8

    for (i <- 0 to 256) {
      oamData(this.oamAddress) = dmaRead(address)
      this.oamAddress = this.oamAddress + this.oamAddress
      address = address + address
    }

    DMAStall = true
  }

  private def nmiChange() = {
    val nmi = this.nmiOutput && this.nmiOccurred
    if (nmi && !this.nmiPrevious) {
      // TODO: this fixes some games but the delay shouldn't have to be so long, so the timings are off somewhere
      this.nmiDelay = 15
    }
    this.nmiPrevious = nmi
  }

  def ReadRegister(address: Int): Int = address match {
    case 0x2002 => readStatus()
    case 0x2004 => readOAMData()
    case 0x2007 => readData()
    case default => 0
  }

  def WriteRegister(address: Int, value: Int, dmaRead: Int => Int) = {
    this.register = value
    address match {
      case 0x2000 => writeControl(value)
      case 0x2001 => writeMask(value)
      case 0x2003 => writeOAMAddress(value)
      case 0x2004 => writeOAMData(value)
      case 0x2005 => writeScroll(value)
      case 0x2006 => writeAddress(value)
      case 0x2007 => writeData(value)
      case 0x4014 => writeDMA(value, dmaRead)
    }
  }

  // tick updates Cycle, ScanLine and Frame counters
  private def tick(cpu: CPU): Unit = {
    if (this.nmiDelay > 0) {
      this.nmiDelay = this.nmiDelay - 1
      if (this.nmiDelay == 0 && this.nmiOutput && this.nmiOccurred) {
        cpu.triggerNMI()
      }
    }

    if (this.flagShowBackground != 0 || this.flagShowSprites != 0) {
      if (this.f == 1 && this.scanLine == 261 && this.cycle == 339) {
        this.cycle = 0
        this.scanLine = 0
        this.frame += 1
        this.f ^= 1
        return
      }
    }

    this.cycle += 1

    if (this.cycle > 340) {
      this.cycle = 0
      this.scanLine += 1
      if (this.scanLine > 261) {
        this.scanLine = 0
        this.frame += 1
        this.f ^= 1
      }
    }
  }

  private val currentTileData: Int = this.tileData >> 32

  private def spritePixel(): (Int, Int) = {
    if (this.flagShowSprites != 0) {
      val default = for {i <- 0 to this.spriteCount
                         offset = (this.cycle - 1) - this.spritePositions(i)
                         nextOffset = 7 - offset if offset > 0 && offset < 7
                         color = this.spritePatterns(i) >> (offset * 4) & 0x0F
                         if color % 4 != 0
                    } yield (i, color)

      default.head
    } else (0, 0)
  }

  private def renderPixel() {
    val x = this.cycle - 1
    val y = this.scanLine
    var background = backgroundPixel()
    var (i, sprite) = spritePixel()

    if (x < 8 && this.flagShowLeftBackground == 0) {
      background = 0
    }

    if (x < 8 && this.flagShowLeftSprites == 0) {
      sprite = 0
    }
    val b = background % 4 != 0
    val s = sprite % 4 != 0
    var color = 0
    if (!b && !s) color = 0
    else if (!b && s) color = sprite | 0x10
    else if (b && !s) color = background
    else {
      if (this.spriteIndexes(i) == 0 && x < 255) {
        this.flagSpriteZeroHit = 1
      }
      if (this.spritePriorities(i) == 0) {
        color = sprite | 0x10
      } else {
        color = background
      }
    }
    val c = Palette.lookup(memory.ReadPalette(color) % 64)
    this.back.setRGB(x, y, c)
  }

  def Step(cpu: CPU): Unit = {
    tick(cpu)

    val renderingEnabled = this.flagShowBackground != 0 || this.flagShowSprites != 0
    val preLine = this.scanLine == 261
    val visibleLine = this.scanLine < 240
    //val postLine = ppu.ScanLine == 240
    val renderLine = preLine || visibleLine
    val preFetchCycle = this.cycle >= 321 && this.cycle <= 336
    val visibleCycle = this.cycle >= 1 && this.cycle <= 256
    val fetchCycle = preFetchCycle || visibleCycle
    // background logic
    if (renderingEnabled) {
      if (visibleLine && visibleCycle) renderPixel()
      if (renderLine && fetchCycle) {
        this.tileData <<= 4
        this.cycle % 8 match {
          case 1 => fetchNameTableByte()
          case 3 => fetchAttributeTableByte()
          case 5 => fetchLowTileByte()
          case 7 => fetchHighTileByte()
          case 0 => storeTileData()
        }
      }
      if (preLine && this.cycle >= 280 && this.cycle <= 304) copyY()

      if (renderLine) {
        if (fetchCycle && this.cycle % 8 == 0) incrementX()
        if (this.cycle == 256) incrementY()
        if (this.cycle == 257) copyX()
      }
    }

    // sprite logic
    if (renderingEnabled) {
      if (this.cycle == 257) {
        if (visibleLine) evaluateSprites()
        else this.spriteCount = 0
      }
    }

    // vblank logic
    if (this.scanLine == 241 && this.cycle == 1) setVerticalBlank()
    if (preLine && this.cycle == 1) {
      clearVerticalBlank()
      this.flagSpriteZeroHit = 0
      this.flagSpriteOverflow = 0
    }
  }

  def fetchNameTableByte() {
    val address = 0x2000 | (this.v & 0x0FFF)
    this.nameTableByte = memory.Read(address)
  }

  def fetchAttributeTableByte() {
    val v = this.v
    val address = 0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)
    val shift = ((v >> 4) & 4) | (v & 2)
    this.attributeTableByte = ((memory.Read(address) >> shift) & 3) << 2
  }

  def fetchLowTileByte() {
    val fineY = (this.v >> 12) & 7
    val table = this.flagBackgroundTable
    val tile = this.nameTableByte
    val address = 0x1000 * (table & 0xFF) + (tile & 0xFF) * 16 + fineY
    this.lowTileByte = memory.Read(address)
  }

  def fetchHighTileByte() {
    val fineY = (this.v >> 12) & 7
    val table = this.flagBackgroundTable
    val tile = this.nameTableByte
    val address = 0x1000 * (table & 0xFF) + (tile & 0xFF) * 16 + fineY
    this.highTileByte = memory.Read(address + 8)
  }

  def storeTileData() {
    var data = 0
    for (i <- 0 to 8) {
      val a = this.attributeTableByte
      val p1 = (this.lowTileByte & 0x80) >> 7
      val p2 = (this.highTileByte & 0x80) >> 6
      this.lowTileByte <<= 1
      this.highTileByte <<= 1
      data = data << 4
      data = data | (a | p1 | p2)
    }
    this.tileData = this.tileData | data
  }

  val fetchTileData = this.tileData >> 32

  def backgroundPixel(): Byte = {
    if (this.flagShowBackground == 0) return 0
    val data = fetchTileData >> ((7 - this.x) * 4)
    (data & 0x0F).toByte
  }

  def copyY() {
    // vert(v) = vert(t)
    // v: .IHGF.ED CBA..... = t: .IHGF.ED CBA.....
    this.v = (this.v & 0x841F) | (this.t & 0x7BE0)
  }

  def incrementX() {
    // increment hori(v)
    // if coarse X == 31
    if ((this.v & 0x001F) == 31) {
      // coarse X = 0
      this.v &= 0xFFE0
      // switch horizontal nametable
      this.v ^= 0x0400
    } else this.v += 1
  }

  def incrementY() {
    // increment vert(v)
    // if fine Y < 7
    if ((this.v & 0x7000) != 0x7000) {
      // increment fine Y
      this.v += 0x1000
    } else {
      // fine Y = 0
      this.v &= 0x8FFF
      // let y = coarse Y
      val testY = (this.v & 0x03E0) >> 5
      val y = if (testY == 29) {
        // switch vertical nametable
        this.v ^= 0x0800
        // coarse Y = 0
        0
      } else if (testY == 31) {
        // coarse Y = 0, nametable not switched
        0
      } else {
        // increment coarse Y
        testY + 1
      }
      // put coarse Y back into v
      this.v = (this.v & 0xFC1F) | (y << 5)
    }
  }

  def copyX() {
    // hori(v) = hori(t)
    // v: .....F.. ...EDCBA = t: .....F.. ...EDCBA
    this.v = (this.v & 0xFBE0) | (this.t & 0x041F)
  }

  def evaluateSprites() {

    val h = if (this.flagSpriteSize == 0) 8
            else 16
    var count = 0
    for (i <- 0 to 63) {
      val y = this.oamData(i * 4 + 0)
      val a = this.oamData(i * 4 + 2)
      val x = this.oamData(i * 4 + 3)
      val row = this.scanLine - y
      if (row > 0 && row <= h)
      if (count < 8) {
        this.spritePatterns(count) = fetchSpritePattern(i, row)
        this.spritePositions(count) = x
        this.spritePriorities(count) = (a >> 5) & 1
        this.spriteIndexes(count) = i & 0xFF
      }
      count += 1
    }
    if (count > 8) {
      count = 8
      this.flagSpriteOverflow = 1
    }
    this.spriteCount = count
  }

  def fetchSpritePattern(i:Int, initialRow:Int):Int = {
    val initialTile = this.oamData(i * 4 + 1)
    val attributes = this.oamData(i * 4 + 2)
    var address = 0
    var row = initialRow
    if (this.flagSpriteSize == 0) {
      if ((attributes & 0x80) == 0x80) row = 7 - row
      val table = this.flagSpriteTable
      address = 0x1000 * table + initialTile * 16 + row
    } else {
      if ((attributes & 0x80) == 0x80) row = 15 - row
      val table = initialTile & 1
      var tile = initialTile & 0xFE
      if (row > 7) {
          tile += 1
          row = row - 8
      }
      address = 0x1000 * table + tile * 16 + row
    }
    val a = (attributes & 3) << 2
    lowTileByte = memory.Read(address)
    highTileByte = memory.Read(address + 8)
    var data = 0
    for (i <- 0 to 7) {
      var p1, p2 = 0
      if ((attributes & 0x40) == 0x40) {
        p1 = (lowTileByte & 1) << 0
        p2 = (highTileByte & 1) << 1
        lowTileByte >>= 1
        highTileByte >>= 1
      } else {
        p1 = (lowTileByte & 0x80) >> 7
        p2 = (highTileByte & 0x80) >> 6
        lowTileByte <<= 1
        highTileByte <<= 1
      }
      data <<= 4
      data |= (a | p1 | p2)
    }
    data
  }

  def setVerticalBlank() {
    this.front = this.back
    this.back = this.front
    this.nmiOccurred = true
    nmiChange()
  }

  def clearVerticalBlank() {
    this.nmiOccurred = false
    nmiChange()
  }
}

object PPU {
  // Reset resets the PPU to its initial power-up state
  def apply(cartridge: Cartridge, mapper:Mapper): PPU = {
    val ppu = new PPU(PPUMemory(cartridge, mapper))
    ppu.Reset()
    ppu
  }
}