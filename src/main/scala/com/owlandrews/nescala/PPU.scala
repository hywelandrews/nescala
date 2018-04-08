package com.owlandrews.nescala

import java.awt.image.BufferedImage

private object PPU {
  private lazy val front  = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)
  private lazy val back   = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)
}

case class PPU(cartridge:Cartridge, mapper:Mapper) extends PPUMemory {

  var DMAStall = false

  private var cycle = 0
  // 0-340
  private var scanLine = 0
  // 0-261, 0-239=visible, 240=post, 241-260=vblank, 261=pre
  private var frame = 0L // frame counter

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

  def ShowBackground = flagShowBackground

  def ShowSprites = flagShowSprites

  def ScanLine = scanLine

  def SpriteSize = flagSpriteSize

  def Cycle = cycle

  def Frame = frame

  def Front = PPU.front

//  def ReInit = {
//    front = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)
//    back = new BufferedImage(256, 240, BufferedImage.TYPE_INT_RGB)
//  }

  // $2000: PPUCTRL
  private def writeControl(value: Int) = {
    flagNameTable = ((value >>> 0) & 3) as uByte
    flagIncrement = ((value >>> 2) & 1)  as uByte
    flagSpriteTable = ((value >>> 3) & 1) as uByte
    flagBackgroundTable = ((value >>> 4) & 1) as uByte
    flagSpriteSize = ((value >>> 5) & 1) as uByte
    flagMasterSlave = ((value >>> 6) & 1) as uByte
    nmiOutput = (((value >>> 7) & 1) as uByte) == 1
    nmiChange()
    // tempVramAddress: ....BA.. ........ = d: ......BA
    tempVramAddress = ((tempVramAddress & 0xF3FF) | ((value as uByte) & 0x03) << 10) as uShort
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
    result = (result | (flagSpriteOverflow << 5)) as uByte
    result = (result | (flagSpriteZeroHit << 6)) as uByte
    if (nmiOccurred) result = (result | (1 << 7)) as uByte
    nmiOccurred = false
    nmiChange()
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
    oamAddress = (oamAddress + 1) as uByte
  }

  // $2005: PPUSCROLL
  private def writeScroll(value: Int) = {
    if (writeToggle == 0) {
      // tempVramAddress: ........ ...HGFED = d: HGFED...
      // fineX:               CBA = d: .....CBA
      // writeToggle:                   = 1
      tempVramAddress = ((tempVramAddress & 0xFFE0) | (value >>> 3)) as uShort
      fineX = (value as uByte) & 0x07
      writeToggle = 1
    } else {
      // tempVramAddress: .CBA..HG FED..... = d: HGFEDCBA
      // writeToggle:                   = 0
      tempVramAddress = ((tempVramAddress & 0x8FFF) | ((value as uShort) & 0x07) << 12) as uShort
      tempVramAddress = ((tempVramAddress & 0xFC1F) | ((value as uShort) & 0xF8) << 2) as uShort
      writeToggle = 0
    }
  }

  // $2006: PPUADDR
  private def writeAddress(value: Int) = {
    if (writeToggle == 0) {
      // tempVramAddress: ..FEDCBA ........ = d: ..FEDCBA
      // tempVramAddress: .X...... ........ = 0
      // writeToggle:                   = 1
      tempVramAddress = ((tempVramAddress & 0x80FF) | ((value  as uShort) & 0x3F) << 8) as uShort
      writeToggle = 1
    } else {
      // tempVramAddress: ........ HGFEDCBA = d: HGFEDCBA
      // vramAddress                    = tempVramAddress
      // writeToggle:                   = 0
      tempVramAddress = ((tempVramAddress & 0xFF00) | (value as uShort)) as uShort
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
    } else bufferedData = Read((vramAddress - 0x1000) as uShort)
    // increment address
    vramAddress = (if (flagIncrement == 0) vramAddress + 1 else vramAddress + 32) as uShort
    value
  }

  // $2007: PPUDATA (write)
  private def writeData(value: Int) = {
    Write(vramAddress, value)
    vramAddress = (if (flagIncrement == 0) vramAddress + 1 else vramAddress + 32) as uShort
  }

  // $4014: OAMDMA
  private def writeDMA(value: Int, dmaRead: Int => Int) = {
    var address = (value << 8) as uShort

    for (i <- 0 to 255) {
      oamData(oamAddress) = dmaRead(address)
      oamAddress = (oamAddress + 1) as uByte
      address = (address + 1) as uShort
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
    case _ => 0
  }

  def WriteRegister(address: Int, value: Int, dmaRead: Int => Int) = {
    register = value as uByte
    address match {
      case 0x2000   => writeControl(value)
      case 0x2001   => writeMask(value)
      case 0x2003   => writeOAMAddress(value)
      case 0x2004   => writeOAMData(value)
      case 0x2005   => writeScroll(value)
      case 0x2006   => writeAddress(value)
      case 0x2007   => writeData(value)
      case 0x4014   => writeDMA(value, dmaRead)
      case _ =>
    }
  }

  // tick updates Cycle, ScanLine and Frame counters
  private def tick(triggerNMIHandler: => Unit): Unit = {
    if (nmiDelay > 0) {
      nmiDelay = (nmiDelay - 1) as uByte
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

  private val emptyPixel = (0, 0)

  private def spritePixel(): (Int, Int) = {

    if(flagShowSprites == 0) return emptyPixel

    for (i <- 0 until spriteCount) {
             val offset = (cycle - 1) - spritePositions(i)
             if (offset >= 0 && offset <= 7) {
               val nextOffset = 7 - offset
               val color = (spritePatterns(i) >> ((nextOffset * 4) as uByte) & 0x0F) as uByte
               if (color % 4 != 0) return (i, color)
             }
    }

    emptyPixel
  }

  private def renderPixel() {
    val x = cycle - 1
    val y = scanLine
    var background = backgroundPixel()
    var (i, sprite) = spritePixel()

    if (x < 8 && flagShowLeftBackground == 0) background = 0

    if (x < 8 && flagShowLeftSprites == 0) sprite = 0

    val backgroundRendering = background % 4 != 0
    val spriteRendering = sprite % 4 != 0
    var color = 0

    if (!backgroundRendering && !spriteRendering) color = 0
    else if (!backgroundRendering && spriteRendering) color = (sprite | 0x10) as uByte
    else if (backgroundRendering && !spriteRendering) color = background
    else {
      if (spriteIndexes(i) == 0 && x < 255) flagSpriteZeroHit = 1
      color = if (spritePriorities(i) == 0) (sprite | 0x10) as uByte else background
    }

    val c = Palette.lookup((ReadPalette(color) as uShort) % 64)
    PPU.back.setRGB(x, y, c)
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
        tileData = (tileData << 4) as uLong

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
    val address = (0x2000 | (vramAddress & 0x0FFF)) as uShort
    nameTableByte = Read(address)
  }

  def fetchAttributeTableByte() {
    val address = (0x23C0 | (vramAddress & 0x0C00) | ((vramAddress >>> 4) & 0x38) | ((vramAddress >>> 2) & 0x07)) as uShort
    val shift = (((vramAddress >> 4) & 4) | (vramAddress & 2)) as uShort
    attributeTableByte = (((Read(address) >>> shift) & 3) << 2) as uByte
  }

  def fetchLowTileByte() {
    val fineY = (vramAddress >> 12) & 7
    val table = flagBackgroundTable
    val tile = nameTableByte
    val address = (((0x1000 * table) as uShort) + (tile * 16 + fineY) as uShort) as uShort
    lowTileByte = Read(address)
  }

  def fetchHighTileByte() {
    val fineY = (vramAddress >> 12) & 7
    val table = flagBackgroundTable
    val tile = nameTableByte
    val address = (((0x1000 * table) as uShort) + (tile * 16 + fineY) as uShort) as uShort
    highTileByte = Read((address + 8) as uShort)
  }

  def storeTileData() {
    var data = 0L
    for (i <- 0 to 7) {
      val a = attributeTableByte
      val p1 = (lowTileByte & 0x80) >> 7
      val p2 = (highTileByte & 0x80) >> 6
      lowTileByte = (lowTileByte << 1) as uByte
      highTileByte = (highTileByte << 1) as uByte
      data = (data << 4) as uInt
      data |= (a | p1 | p2) as uInt
    }

    tileData = tileData | (data as uLong)
  }

  def backgroundPixel(): Int = {
    if (flagShowBackground == 0) return 0
    val fetchTileData = tileData >>> 32
    val data = fetchTileData >> ((((7 - fineX) as uByte) * 4) as uByte)
    (data & 0x0F).toInt as uByte
  }

  def copyY() {
    // vert(vramAddress) = vert(tempVramAddress)
    // vramAddress: .IHGF.ED CBA..... = tempVramAddress: .IHGF.ED CBA.....
    vramAddress = ((vramAddress & 0x841F) | (tempVramAddress & 0x7BE0)) as uShort
  }

  def incrementX() {
    // increment hori(vramAddress)
    // if coarse X == 31
    if ((vramAddress & 0x001F) == 31) {
      // coarse X = 0
      vramAddress &= ~0x001F
      // switch horizontal nametable
      vramAddress ^= 0x0400
    } else vramAddress = (vramAddress + 1) as uShort
  }

  def incrementY() {
    // increment vert(vramAddress)
    // if fine Y < 7
    if ((vramAddress & 0x7000) != 0x7000) {
      // increment fine Y
      vramAddress = (vramAddress + 0x1000) as uShort
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
        (testY + 1) as uShort
      }
      // put coarse Y back into vramAddress
      vramAddress = ((vramAddress & 0xFC1F) | (y << 5)) as uShort
    }
  }

  def copyX() {
    // hori(vramAddress) = hori(tempVramAddress)
    // vramAddress: .....F.. ...EDCBA = tempVramAddress: .....F.. ...EDCBA
    vramAddress = ((vramAddress & 0xFBE0) | (tempVramAddress & 0x041F)) as uShort
  }

  var isATransparentSprite = 0

  def evaluateSprites() {

    val h = if (flagSpriteSize == 0) 8
            else 16
    var count = 0

    for(i <- 0 until 64) {
      val y = oamData(i * 4 + 0) as uByte
      val a = oamData(i * 4 + 2) as uByte
      val x = oamData(i * 4 + 3) as uByte
      val row = scanLine - y

      if (row >= 0 && row < h) {
        if (count < 8) {
          spritePatterns(count) = fetchSpritePattern(i, row)
          spritePositions(count) = x
          spritePriorities(count) = (a >>> 5) & 1
          spriteIndexes(count) = i as uByte
        }
        count += 1
      }
    }

    if (count > 8) {
      count = 8
      flagSpriteOverflow = 1
    }
    spriteCount = count
  }

  def fetchSpritePattern(i:Int, initialRow:Int):Int = {
    val initialTile = oamData(i * 4 + 1) as uByte
    val attributes = oamData(i * 4 + 2) as uByte
    var address = 0
    var row = initialRow
    if (flagSpriteSize == 0) {
      if ((attributes & 0x80) == 0x80) row = 7 - row
      val table = flagSpriteTable
      address = (0x1000 * (table as uShort) + (initialTile as uShort) * 16 + (row as uShort)) as uShort
    } else {
      if ((attributes & 0x80) == 0x80) row = 15 - row
      val table = initialTile & 1
      var tile = initialTile & 0xFE
      if (row > 7) {
          tile = (tile + 1) as uByte
          row -= 8
      }
      address = (((0x1000 * table) as uShort) + ((tile * 16) as uShort) + row) as uShort
    }
    val a = (attributes & 3) << 2
    lowTileByte = Read(address) as uByte
    highTileByte = Read((address + 8) as uShort) as uByte
    var data = 0
    for (i <- 0 until 8) {
      var p1, p2 = 0
      if ((attributes & 0x40) == 0x40) {
        p1 = ((lowTileByte & 1) << 0) as uByte
        p2 = ((highTileByte & 1) << 1) as uByte
        lowTileByte = (lowTileByte >> 1) as uByte
        highTileByte = (highTileByte >> 1) as uByte
      } else {
        p1 = ((lowTileByte & 0x80) >>> 7) as uByte
        p2 = ((highTileByte & 0x80) >>> 6) as uByte
        lowTileByte = (lowTileByte << 1) as uByte
        highTileByte = (highTileByte << 1) as uByte
      }
      data <<= 4
      data |= (a | p1 | p2) & 0xFFFFFFFF
    }

    data
  }

  def setVerticalBlank() {
    PPU.front.setData(PPU.back.getRaster)
    nmiOccurred = true
    nmiChange()
  }

  def clearVerticalBlank() {
    nmiOccurred = false
    nmiChange()
  }

  Reset()
}