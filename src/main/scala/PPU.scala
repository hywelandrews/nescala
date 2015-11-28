package nescala

import java.awt.image.BufferedImage

case class PPU(cartridge:Cartridge, mapper:Mapper) extends PPUMemory {

  var DMAStall = false

  private var cycle = 0
  // 0-340
  var scanLine = 0
  // 0-261, 0-239=visible, 240=post, 241-260=vblank, 261=pre
  var frame = 0L // frame counter

  var front = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)
  private val back = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)

  private val oamData = Array.fill(256)(0)
  // PPU registers
  // current vram address (15 bit) // temporary vram address (15 bit) // fine fineX scroll (3 bit) // write toggle (1 bit) // even/odd frame flag (1 bit)
  private var vramAddress, tempVramAddress, fineX, writeToggle, frameFlag = 0
  private var register = 0

  // NMI flags
  private var nmiOccurred, nmiOutput, nmiPrevious = false
  private var nmiDelay = 0

  // background temporary variables
  private var nameTableByte, attributeTableByte, lowTileByte, highTileByte = 0

  private var tileData = 0L

  // sprite temporary variables
  private var spriteCount = 0
  private val spritePatterns, spritePositions, spritePriorities, spriteIndexes =  Array.fill(8)(0)

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
    cycle = 340
    scanLine = 240
    frame = 0
    writeControl(0)
    writeMask(0)
    writeOAMAddress(0)
  }

  // $2000: PPUCTRL
  private def writeControl(value: Int) = {
    flagNameTable = ((value >>> 0) & 3) & 0xFF
    flagIncrement = ((value >>> 2) & 1)  & 0xFF
    flagSpriteTable = ((value >>> 3) & 1)  & 0xFF
    flagBackgroundTable = ((value >>> 4) & 1)  & 0xFF
    flagSpriteSize = ((value >>> 5) & 1)  & 0xFF
    flagMasterSlave = ((value >>> 6) & 1)  & 0xFF
    nmiOutput = (((value >>> 7) & 1) & 0xFF) == 1
    nmiChange()
    // tempVramAddress: ....BA.. ........ = d: ......BA
    tempVramAddress = ((tempVramAddress & 0xF3FF) | (value & 0xFF & 0x03) << 10) & 0xFFFF
  }

  // $2001: PPUMASK
  private def writeMask(value: Int) = {
    flagGrayscale = (value >>> 0) & 1
    flagShowLeftBackground = (value >>> 1) & 1
    flagShowLeftSprites = (value >>> 2) & 1
    flagShowBackground = (value >>> 3) & 1
    flagShowSprites = (value >>> 4) & 1
    flagRedTint = (value >>> 5) & 1
    flagGreenTint = (value >>> 6) & 1
    flagBlueTint = (value >>> 7) & 1
  }

  // $2002: PPUSTATUS
  private def readStatus(): Int = {
    var result = register & 0x1F
    result = (result | (flagSpriteOverflow << 5)) & 0xFF
    result = (result | (flagSpriteZeroHit << 6)) & 0xFF
    if (nmiOccurred) result = (result | (1 << 7)) & 0xFF
    nmiOccurred = false
    nmiChange()
    // writeToggle:     = 0
    writeToggle = 0
    result
  }

  // $2003: OAMADDR
  private def writeOAMAddress(value: Int) = oamAddress = value

  // $2004: OAMDATA (read)
  private def readOAMData(): Int = oamData(oamAddress)

  // $2004: OAMDATA (write)
  private def writeOAMData(value: Int) = {
    oamData(oamAddress) = value
    oamAddress = (oamAddress + 1) & 0xFF
  }

  // $2005: PPUSCROLL
  private def writeScroll(value: Int) = {
    if (writeToggle == 0) {
      // tempVramAddress: ........ ...HGFED = d: HGFED...
      // fineX:               CBA = d: .....CBA
      // writeToggle:                   = 1
      tempVramAddress = ((tempVramAddress & 0xFFE0) | (value >>> 3)) & 0xFFFF
      fineX = value & 0xFF & 0x07
      writeToggle = 1
    } else {
      // tempVramAddress: .CBA..HG FED..... = d: HGFEDCBA
      // writeToggle:                   = 0
      tempVramAddress = ((tempVramAddress & 0x8FFF) | ((value & 0xFFFF & 0x07) << 12)) & 0xFFFF
      tempVramAddress = ((tempVramAddress & 0xFC1F) | ((value & 0xFFFF & 0xF8) << 2)) & 0xFFFF
      writeToggle = 0
    }
  }

  // $2006: PPUADDR
  private def writeAddress(value: Int) = {
    if (writeToggle == 0) {
      // tempVramAddress: ..FEDCBA ........ = d: ..FEDCBA
      // tempVramAddress: .X...... ........ = 0
      // writeToggle:                   = 1
      tempVramAddress = ((tempVramAddress & 0x80FF) | ((value  & 0xFFFF & 0x3F) << 8)) & 0xFFFF
      writeToggle = 1
    } else {
      // tempVramAddress: ........ HGFEDCBA = d: HGFEDCBA
      // vramAddress                    = tempVramAddress
      // writeToggle:                   = 0
      tempVramAddress = ((tempVramAddress & 0xFF00) | (value  & 0xFFFF)) & 0xFFFF
      vramAddress = tempVramAddress
      writeToggle = 0
    }
  }

  // $2007: PPUDATA (read)
  private def readData(): Int = {
    var value = Read(vramAddress)
    // emulate buffered reads
    if (vramAddress % 0x4000 < 0x3F00) {
      val buffered = bufferedData
      bufferedData = value
      value = buffered
    } else bufferedData = Read((vramAddress - 0x1000) & 0xFFFF)
    // increment address
    vramAddress = (if (flagIncrement == 0) vramAddress + 1 else vramAddress + 32) & 0xFFFF
    value
  }

  // $2007: PPUDATA (write)
  private def writeData(value: Int) = {
    Write(vramAddress, value)
    vramAddress = (if (flagIncrement == 0) vramAddress + 1 else vramAddress + 32) & 0xFFFF
  }

  // $4014: OAMDMA
  private def writeDMA(value: Int, dmaRead: Int => Int) = {
    var address = (value << 8) & 0xFFFF

    for (i <- 0 to 255) {
      oamData(oamAddress) = dmaRead(address)
      oamAddress = (oamAddress + 1) & 0xFF
      address = (address + 1) & 0xFFFF
    }

    DMAStall = true
  }

  private def nmiChange() = {
    val nmi = nmiOutput && nmiOccurred
    if (nmi && !nmiPrevious) {
      // TODO: this fixes the odd game, but is incorrect
      nmiDelay = 15
    }
    nmiPrevious = nmi
  }

  def ReadRegister(address: Int): Int = address match {
    case 0x2002 => readStatus()
    case 0x2004 => readOAMData()
    case 0x2007 => readData()
    case default => 0
  }

  def WriteRegister(address: Int, value: Int, dmaRead: Int => Int) = {
    register = value & 0xFF
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
  private def tick(triggerNMIHandler: => Unit): Unit = {
    if (nmiDelay > 0) {
      nmiDelay = (nmiDelay - 1) & 0xFF
      if (nmiDelay == 0 && nmiOutput && nmiOccurred) triggerNMIHandler
    }

    if (flagShowBackground != 0 || flagShowSprites != 0) {
      if (frameFlag == 1 && scanLine == 261 && cycle == 339) {
        cycle = 0
        scanLine = 0
        frame += 1
        frameFlag ^= 1
        return
      }
    }

    cycle += 1

    if (cycle > 340) {
      cycle = 0
      scanLine += 1
      if (scanLine > 261) {
        scanLine = 0
        frame += 1
        frameFlag ^= 1
      }
    }
  }

  private def spritePixel(): (Int, Int) = {
    if (flagShowSprites != 0) {
      val default = for {i <- 0 until spriteCount
                         offset = (cycle - 1) - spritePositions(i)
                         if offset >= 0 && offset <= 7
                         nextOffset = 7 - offset
                         color = (spritePatterns(i) >> ((nextOffset * 4) & 0xFF) & 0x0F) & 0xFF
                         if color % 4 != 0
                    } yield (i, color)

      default.foldLeft((0,0)){ case (any, (i, color)) => (i, color)}
    } else (0, 0)
  }

  private def renderPixel() {
    val x = cycle - 1
    val y = scanLine
    var background = backgroundPixel()
    var (i, sprite) = spritePixel()

    if (x < 8 && flagShowLeftBackground == 0) background = 0

    if (x < 8 && flagShowLeftSprites == 0) sprite = 0

    val b = background % 4 != 0
    val s = sprite % 4 != 0
    var color = 0

    if (!b && !s) color = 0
    else if (!b && s) color = sprite | 0x10
    else if (b && !s) color = background
    else {
      if (spriteIndexes(i) == 0 && x < 255) flagSpriteZeroHit = 1
      if (spritePriorities(i) == 0) color = (sprite | 0x10) & 0xFF
      else color = background
    }
    val c = Palette.lookup((ReadPalette(color) & 0xFF) % 64)
    back.setRGB(x, y, c)
  }

  def Step(triggerNMIHandler: => Unit): Unit = {
    tick(triggerNMIHandler)

    val renderingEnabled = flagShowBackground != 0 || flagShowSprites != 0
    val preLine = scanLine == 261
    val visibleLine = scanLine < 240
    //val postLine = scanLine == 240
    val renderLine = preLine || visibleLine
    val preFetchCycle = cycle >= 321 && cycle <= 336
    val visibleCycle = cycle >= 1 && cycle <= 256
    val fetchCycle = preFetchCycle || visibleCycle

    // background logic
    if (renderingEnabled) {

      if (visibleLine && visibleCycle) {
        renderPixel()
      }
      if (renderLine && fetchCycle) {
        tileData = (tileData << 4) & 0xFFFFFFFFFFFFFFFFL

        cycle % 8 match {
          case 1 => fetchNameTableByte()
          case 3 => fetchAttributeTableByte()
          case 5 => fetchLowTileByte()
          case 7 => fetchHighTileByte()
          case 0 => storeTileData()
          case _ =>
        }
      }
      if (preLine && cycle >= 280 && cycle <= 304) copyY()

      if (renderLine) {
        if (fetchCycle && cycle % 8 == 0) incrementX()
        if (cycle == 256) incrementY()
        if (cycle == 257) copyX()
      }
    }

    // sprite logic
    if (renderingEnabled) {
      if (cycle == 257) {
        if (visibleLine) evaluateSprites()
        else spriteCount = 0
      }
    }

    // vblank logic
    if (scanLine == 241 && cycle == 1) setVerticalBlank()
    if (preLine && cycle == 1) {
      clearVerticalBlank()
      flagSpriteZeroHit = 0
      flagSpriteOverflow = 0
    }
  }

  def fetchNameTableByte() {
    val address = (0x2000 | (vramAddress & 0x0FFF)) & 0xFFFF
    nameTableByte = Read(address)
  }

  def fetchAttributeTableByte() {
    val address = (0x23C0 | (vramAddress & 0x0C00) | ((vramAddress >>> 4) & 0x38) | ((vramAddress >>> 2) & 0x07)) & 0xFFFF
    val shift = (((vramAddress >> 4) & 4) | (vramAddress & 2)) & 0xFFFF
    attributeTableByte = (((Read(address) >>> shift) & 3) << 2) & 0xFF
  }

  def fetchLowTileByte() {
    val fineY = (vramAddress >> 12) & 7
    val table = flagBackgroundTable
    val tile = nameTableByte
    val address = (((0x1000 * table) & 0xFFFF) + (tile * 16 + fineY) & 0xFFFF) & 0xFFFF
    lowTileByte = Read(address)
  }

  def fetchHighTileByte() {
    val fineY = (vramAddress >> 12) & 7
    val table = flagBackgroundTable
    val tile = nameTableByte
    val address = (((0x1000 * table) & 0xFFFF) + (tile * 16 + fineY) & 0xFFFF) & 0xFFFF
    highTileByte = Read((address + 8) & 0xFFFF)
  }

  def storeTileData() {
    var data = 0L
    for (i <- 0 to 7) {
      val a = attributeTableByte
      val p1 = (lowTileByte & 0x80) >> 7
      val p2 = (highTileByte & 0x80) >> 6
      lowTileByte = (lowTileByte << 1) & 0xFF
      highTileByte = (highTileByte << 1) & 0xFF
      data = (data << 4) & 0xFFFFFFFF
      data |= (a | p1 | p2) & 0xFFFFFFFF
    }

    tileData = tileData | (data & 0xFFFFFFFFFFFFFFFFL)
  }

  def fetchTileData = (tileData >> 32) & 0xFFFFFFFFFFFFFFFFL

  def backgroundPixel(): Int = {
    if (flagShowBackground == 0) return 0
    val data = (fetchTileData >> ((((7 - fineX) & 0xFF) * 4) & 0xFF)) & 0xFFFFFFFFFFFFFFFFL
    ((data & 0x0F) & 0xFF).toInt
  }

  def copyY() {
    // vert(vramAddress) = vert(tempVramAddress)
    // vramAddress: .IHGF.ED CBA..... = tempVramAddress: .IHGF.ED CBA.....
    vramAddress = ((vramAddress & 0x841F) | (tempVramAddress & 0x7BE0)) & 0xFFFF
  }

  def incrementX() {
    // increment hori(vramAddress)
    // if coarse X == 31
    if ((vramAddress & 0x001F) == 31) {
      // coarse X = 0
      vramAddress &= ~0x001F
      // switch horizontal nametable
      vramAddress ^= 0x0400
    } else vramAddress = (vramAddress + 1) & 0xFFFF
  }

  def incrementY() {
    // increment vert(vramAddress)
    // if fine Y < 7
    if ((vramAddress & 0x7000) != 0x7000) {
      // increment fine Y
      vramAddress = (vramAddress + 0x1000) & 0xFFFF
    } else {
      // fine Y = 0
      vramAddress &= 0x8FFF
      // let y = coarse Y
      val testY = (vramAddress & 0x03E0) >>> 5
      val y = if (testY == 29) {
        // switch vertical nametable
        vramAddress ^= 0x0800
        // coarse Y = 0
        0
      } else if (testY == 31) {
        // coarse Y = 0, nametable not switched
        0
      } else {
        // increment coarse Y
        (testY + 1) & 0xFFFF
      }
      // put coarse Y back into vramAddress
      vramAddress = ((vramAddress & 0xFC1F) | (y << 5)) & 0xFFFF
    }
  }

  def copyX() {
    // hori(vramAddress) = hori(tempVramAddress)
    // vramAddress: .....F.. ...EDCBA = tempVramAddress: .....F.. ...EDCBA
    vramAddress = ((vramAddress & 0xFBE0) | (tempVramAddress & 0x041F)) & 0xFFFF
  }

  def evaluateSprites() {

    val h = if (flagSpriteSize == 0) 8
            else 16
    var count = 0

    for {
      i <- 0 until 64
      y = oamData(i * 4 + 0) & 0xFF
      a = oamData(i * 4 + 2) & 0xFF
      x = oamData(i * 4 + 3) & 0xFF
      row = scanLine - y
      if row >= 0 && row < h
    } yield {
        if (count < 8) {
          spritePatterns(count) = fetchSpritePattern(i, row)
          spritePositions(count) = x
          spritePriorities(count) = (a >>> 5) & 1
          spriteIndexes(count) = i & 0xFF
        }
        count += 1
    }

    if (count > 8) {
      count = 8
      flagSpriteOverflow = 1
    }
    spriteCount = count
  }

  def fetchSpritePattern(i:Int, initialRow:Int):Int = {
    val initialTile = oamData(i * 4 + 1) & 0xFF
    val attributes = oamData(i * 4 + 2) & 0xFF
    var address = 0
    var row = initialRow
    if (this.flagSpriteSize == 0) {
      if ((attributes & 0x80) == 0x80) row = 7 - row
      val table = flagSpriteTable
      address = (0x1000 * (table & 0xFFFF) + (initialTile & 0xFFFF) * 16 + (row & 0xFFFF)) & 0xFFFF
    } else {
      if ((attributes & 0x80) == 0x80) row = 15 - row
      val table = initialTile & 1
      var tile = initialTile & 0xFE
      if (row > 7) {
          tile = (tile + 1) & 0xFF
          row -= 8
      }
      address = (((0x1000 * table) & 0xFFFF) + ((tile * 16) & 0xFFFF) + row) & 0xFFFF
    }
    val a = (attributes & 3) << 2
    lowTileByte = Read(address) & 0xFF
    highTileByte = Read((address + 8) & 0xFFFF) & 0xFF
    var data = 0
    for (i <- 0 to 7) {
      var p1, p2 = 0
      if ((attributes & 0x40) == 0x40) {
        p1 = ((lowTileByte & 1) << 0) & 0xFF
        p2 = ((highTileByte & 1) << 1) & 0xFF
        lowTileByte >>>= 1
        highTileByte >>>= 1
      } else {
        p1 = ((lowTileByte & 0x80) >>> 7) & 0xFF
        p2 = ((highTileByte & 0x80) >>> 6) & 0xFF
        lowTileByte = (lowTileByte << 1) & 0xFF
        highTileByte = (highTileByte << 1) & 0xFF
      }
      data <<= 4
      data |= (a | p1 | p2) & 0xFFFFFFFF
    }
    data
  }

  def setVerticalBlank() {
    front = back
    nmiOccurred = true
    nmiChange()
  }

  def clearVerticalBlank() {
    nmiOccurred = false
    nmiChange()
  }

  Reset()
}