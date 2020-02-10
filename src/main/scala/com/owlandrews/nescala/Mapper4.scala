package com.owlandrews.nescala

case class Mapper4(mirror:Int, chrRom:Array[Int], prgRom:Array[Int], sRam:Array[Int]) extends Mapper {

  private val prgOffsets = Array(prgBankOffset(0), prgBankOffset(1), prgBankOffset(-2), prgBankOffset(-1))
  private val chrOffsets = Array.fill[Int](8)(0)
  private val registers = Array.fill[Int](8)(0)

  private var register, prgMode, chrMode, reload, counter    = 0
  private var irqEnable  = false
  private var mirrorMode = mirror

  override def Read(address:Int): Int =  address as uShort match {
    case chr if isChr(address) =>
      val bank = chr / 0x0400
      val offset = chr % 0x0400
      chrRom(chrOffsets(bank) + offset)
    case prg1 if isPrg1(address) =>
      val baseAddress = (prg1 - 0x8000) as uShort
      val bank = baseAddress / 0x2000
      val offset = baseAddress % 0x2000
      prgRom(prgOffsets(bank) + offset)
    case saveRam if address >= 0x6000 =>
      val index = (saveRam - 0x6000) as uShort
      sRam(index)
    case default => System.err.println(s"Unhandled mapper4 write at address: ${Integer.toHexString(default)}"); 0
  }

  override def Write(address: Int, value: Int): Unit = address as uShort match {
    case chr if isChr(address) =>
      val bank = chr / 0x0400
      val offset = chr % 0x0400
      chrRom(chrOffsets(bank) + offset) = value
    case prg1 if isPrg1(address) => writeRegister(prg1, value)
    case saveRam if isSRam(address) =>
      val index = (saveRam - 0x6000)  as uShort
      sRam(index) = value
    case default => System.err.println(s"Unhandled mapper4 read at address: ${Integer.toHexString(default)}"); throw new Exception
  }

  override def Mirror: Int = mirrorMode

  private def writeRegister(address:Int, value:Int) = address match {
    case _ if address <= 0x9FFF && address % 2 == 0 => writeBankSelect(value)
    case _ if address <= 0x9FFF && address % 2 == 1 => writeBankData(value)
    case _ if address <= 0xBFFF && address % 2 == 0 => writeMirror(value)
    case _ if address <= 0xBFFF && address % 2 == 1 => writeProtect(value)
    case _ if address <= 0xDFFF && address % 2 == 0 => writeIRQLatch(value)
    case _ if address <= 0xDFFF && address % 2 == 1 => writeIRQReload(value)
    case _ if address <= 0xFFFF && address % 2 == 0 => writeIRQDisable(value)
    case _ if address <= 0xFFFF && address % 2 == 1 => writeIRQEnable(value)
  }

  private def writeBankSelect(value:Int) = {
    prgMode = (value >> 6) & 1
    chrMode = (value >> 7) & 1
    register = value & 7
    updateOffsets()
  }

  private def writeBankData(value:Int) = {
    registers(register) = value
    updateOffsets()
  }

  private def writeMirror(value:Int) = value & 1 match {
    case 0 => mirrorMode = MirrorMode.Vertical
    case 1 => mirrorMode = MirrorMode.Horizontal
  }

  private def writeProtect(value:Int) = ()

  private def writeIRQLatch(value:Int) = reload = value

  private def writeIRQReload(value:Int) = counter = 0

  private def writeIRQDisable(value:Int) = irqEnable = false

  private def writeIRQEnable(value:Int) = irqEnable = true

  private def chrBankOffset(index: Int): Int = {
    val bankLength = chrRom.length
    val y = if (index >= 0x80) index - 0x100 else index
    val offset = findOffset(y, bankLength, 0x0400)

    if (offset < 0) offset + bankLength
    else offset
  }

  private def prgBankOffset(index: Int): Int = {
    val bankLength = prgRom.length
    val y = if (index >= 0x80) index - 0x100 else index
    val offset = findOffset(y, bankLength, 0x2000)

    if (offset < 0) offset + bankLength
    else offset
  }

  private def findOffset(index: Int, bankLength: Int, size: Int) = (index % (bankLength / size)) * size

  private def updateOffsets() = {

    prgMode match {
      case 0 =>
        prgOffsets(0) = prgBankOffset(registers(6))
        prgOffsets(1) = prgBankOffset(registers(7))
        prgOffsets(2) = prgBankOffset(-2)
        prgOffsets(3) = prgBankOffset(-1)
      case 1 =>
        prgOffsets(0) = prgBankOffset(-2)
        prgOffsets(1) = prgBankOffset(registers(7))
        prgOffsets(2) = prgBankOffset(registers(6))
        prgOffsets(3) = prgBankOffset(-1)
    }

    chrMode match {
      case 0 =>
        chrOffsets(0) = chrBankOffset(registers(0) & 0xFE)
        chrOffsets(1) = chrBankOffset(registers(0) | 0x01)
        chrOffsets(2) = chrBankOffset(registers(1) & 0xFE)
        chrOffsets(3) = chrBankOffset(registers(1) | 0x01)
        chrOffsets(4) = chrBankOffset(registers(2))
        chrOffsets(5) = chrBankOffset(registers(3))
        chrOffsets(6) = chrBankOffset(registers(4))
        chrOffsets(7) = chrBankOffset(registers(5))
      case 1 =>
        chrOffsets(0) = chrBankOffset(registers(2))
        chrOffsets(1) = chrBankOffset(registers(3))
        chrOffsets(2) = chrBankOffset(registers(4))
        chrOffsets(3) = chrBankOffset(registers(5))
        chrOffsets(4) = chrBankOffset(registers(0) & 0xFE)
        chrOffsets(5) = chrBankOffset(registers(0) | 0x01)
        chrOffsets(6) = chrBankOffset(registers(1) & 0xFE)
        chrOffsets(7) = chrBankOffset(registers(1) | 0x01)
    }
  }

  override def Step(ppuCycle:Long, ppuScanLine:Long, ppuFlagShowBackground:Int, ppuFlagShowSprites:Int, flagSpriteSize:Int, triggerIRQHandler: => Unit):Unit = {

    if (ppuCycle != 280) return
    if (ppuScanLine > 239 && ppuScanLine < 261) return
    if (ppuFlagShowBackground == 0 && ppuFlagShowSprites == 0) return
    handleScanLine(triggerIRQHandler)
  }

  private def handleScanLine(triggerIRQHandler: => Unit) {
    if (counter == 0) counter = reload
    else {
      counter = (counter - 1) as uByte
      if (counter == 0 && irqEnable) triggerIRQHandler
    }
  }

}