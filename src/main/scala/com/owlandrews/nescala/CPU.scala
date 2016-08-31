package com.owlandrews.nescala

import scala.language.postfixOps

import helpers.Unsigned._

case class CPU(memory:CPUMemory) {

  import CPU.{addressModes, interrupts}

  // address, pc, mode
  private type Operation = (Int, Int, Byte) => Unit
  // pc: program counter    / sp: stack pointer / a:  accumulator   / x:  register
  // y:  register           / c:  carry flag    / z:  zero flag     / i:  interrupt disable flag / d:  decimal mode flag
  // b:  break command flag / u:  unused flag   / v:  overflow flag / n:  negative flag
  private case class Registers(var pc:Int = initialProgramCounter, var sp:Int = initialStackPointer, var a:Int = 0, var x:Int = 0,
                          var y:Int = 0, var c:Int = 0, var z:Int = 0, var i:Int = 0, var d:Int = 0,
                          var b:Int = 0, var u:Int = 0, var v:Int = 0, var n:Int = 0)

  private val initialProgramCounter:Int = memory.Read(0xFFFD) * 256 + memory.Read(0xFFFC)
  private val initialStackPointer:Int = 0xFD
  // mode: the address mode of instruction
  // size: indicates the size of each instruction in bytes
  // cycles: indicates the number of cycles used by each instruction, not including conditional cycles
  // pageCycles: indicates the number of cycles used by each instruction when a page is crossed
  private case class Instruction(name:String, mode:Byte, size:Byte, cycle:Byte, pageCycles:Byte, op:Operation)

  private val instructions = Vector[Instruction](
    Instruction("BRK", 6, 1, 7, 0, brk),  Instruction("ORA", 7, 2, 6, 0, ora),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("SLO", 7, 2, 8, 0, slo),
    Instruction("NOP", 11, 2, 3, 0, nop), Instruction("ORA", 11, 2, 3, 0, ora), Instruction("ASL", 11, 2, 5, 0, asl), Instruction("SLO", 11, 2, 5, 0, slo),
    Instruction("PHP", 6, 1, 3, 0, php),  Instruction("ORA", 5, 2, 2, 0, ora),  Instruction("ASL", 4, 1, 2, 0, asl),  Instruction("ANC", 5, 0, 2, 0, anc),
    Instruction("NOP", 1, 3, 4, 0, nop),  Instruction("ORA", 1, 3, 4, 0, ora),  Instruction("ASL", 1, 3, 6, 0, asl),  Instruction("SLO", 1, 3, 6, 0, slo),
    Instruction("BPL", 10, 2, 2, 1, bpl), Instruction("ORA", 9, 2, 5, 1, ora),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("SLO", 9, 2, 8, 0, slo),
    Instruction("NOP", 12, 2, 4, 0, nop), Instruction("ORA", 12, 2, 4, 0, ora), Instruction("ASL", 12, 2, 6, 0, asl), Instruction("SLO", 12, 2, 6, 0, slo),
    Instruction("CLC", 6, 1, 2, 0, clc),  Instruction("ORA", 3, 3, 4, 1, ora),  Instruction("NOP", 6, 1, 2, 0, nop),  Instruction("SLO", 3, 3, 7, 0, slo),
    Instruction("NOP", 2, 3, 4, 1, nop),  Instruction("ORA", 2, 3, 4, 1, ora),  Instruction("ASL", 2, 3, 7, 0, asl),  Instruction("SLO", 2, 3, 7, 0, slo),
    Instruction("JSR", 1, 3, 6, 0, jsr),  Instruction("AND", 7, 2, 6, 0, and),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("RLA", 7, 2, 8, 0, rla),
    Instruction("BIT", 11, 2, 3, 0, bit), Instruction("AND", 11, 2, 3, 0, and), Instruction("ROL", 11, 2, 5, 0, rol), Instruction("RLA", 11, 2, 5, 0, rla),
    Instruction("PLP", 6, 1, 4, 0, plp),  Instruction("AND", 5, 2, 2, 0, and),  Instruction("ROL", 4, 1, 2, 0, rol),  Instruction("ANC", 5, 0, 2, 0, anc),
    Instruction("BIT", 1, 3, 4, 0, bit),  Instruction("AND", 1, 3, 4, 0, and),  Instruction("ROL", 1, 3, 6, 0, rol),  Instruction("RLA", 1, 3, 6, 0, rla),
    Instruction("BMI", 10, 2, 2, 1, bmi), Instruction("AND", 9, 2, 5, 1, and),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("RLA", 9, 2, 8, 0, rla),
    Instruction("NOP", 12, 2, 4, 0, nop), Instruction("AND", 12, 2, 4, 0, and), Instruction("ROL", 12, 2, 6, 0, rol), Instruction("RLA", 12, 2, 6, 0, rla),
    Instruction("SEC", 6, 1, 2, 0, sec),  Instruction("AND", 3, 3, 4, 1, and),  Instruction("NOP", 6, 1, 2, 0, nop),  Instruction("RLA", 3, 3, 7, 0, rla),
    Instruction("NOP", 2, 3, 4, 1, nop),  Instruction("AND", 2, 3, 4, 1, and),  Instruction("ROL", 2, 3, 7, 0, rol),  Instruction("RLA", 2, 3, 7, 0, rla),
    Instruction("RTI", 6, 1, 6, 0, rti),  Instruction("EOR", 7, 2, 6, 0, eor),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("SRE", 7, 2, 8, 0, sre),
    Instruction("NOP", 11, 2, 3, 0, nop), Instruction("EOR", 11, 2, 3, 0, eor), Instruction("LSR", 11, 2, 5, 0, lsr), Instruction("SRE", 11, 2, 5, 0, sre),
    Instruction("PHA", 6, 1, 3, 0, pha),  Instruction("EOR", 5, 2, 2, 0, eor),  Instruction("LSR", 4, 1, 2, 0, lsr),  Instruction("ALR", 5, 0, 2, 0, alr),
    Instruction("JMP", 1, 3, 3, 0, jmp),  Instruction("EOR", 1, 3, 4, 0, eor),  Instruction("LSR", 1, 3, 6, 0, lsr),  Instruction("SRE", 1, 3, 6, 0, sre),
    Instruction("BVC", 10, 2, 2, 1, bvc), Instruction("EOR", 9, 2, 5, 1, eor),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("SRE", 9, 2, 8, 0, sre),
    Instruction("NOP", 12, 2, 4, 0, nop), Instruction("EOR", 12, 2, 4, 0, eor), Instruction("LSR", 12, 2, 6, 0, lsr), Instruction("SRE", 12, 2, 6, 0, sre),
    Instruction("CLI", 6, 1, 2, 0, cli),  Instruction("EOR", 3, 3, 4, 1, eor),  Instruction("NOP", 6, 1, 2, 0, nop),  Instruction("SRE", 3, 3, 7, 0, sre),
    Instruction("NOP", 2, 3, 4, 1, nop),  Instruction("EOR", 2, 3, 4, 1, eor),  Instruction("LSR", 2, 3, 7, 0, lsr),  Instruction("SRE", 2, 3, 7, 0, sre),
    Instruction("RTS", 6, 1, 6, 0, rts),  Instruction("ADC", 7, 2, 6, 0, adc),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("RRA", 7, 2, 8, 0, rra),
    Instruction("NOP", 11, 2, 3, 0, nop), Instruction("ADC", 11, 2, 3, 0, adc), Instruction("ROR", 11, 2, 5, 0, ror), Instruction("RRA", 11, 2, 5, 0, rra),
    Instruction("PLA", 6, 1, 4, 0, pla),  Instruction("ADC", 5, 2, 2, 0, adc),  Instruction("ROR", 4, 1, 2, 0, ror),  Instruction("ARR", 5, 0, 2, 0, arr),
    Instruction("JMP", 8, 3, 5, 0, jmp),  Instruction("ADC", 1, 3, 4, 0, adc),  Instruction("ROR", 1, 3, 6, 0, ror),  Instruction("RRA", 1, 3, 6, 0, rra),
    Instruction("BVS", 10, 2, 2, 1, bvs), Instruction("ADC", 9, 2, 5, 1, adc),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("RRA", 9, 2, 8, 0, rra),
    Instruction("NOP", 12, 2, 4, 0, nop), Instruction("ADC", 12, 2, 4, 0, adc), Instruction("ROR", 12, 2, 6, 0, ror), Instruction("RRA", 12, 2, 6, 0, rra),
    Instruction("SEI", 6, 1, 2, 0, sei),  Instruction("ADC", 3, 3, 4, 1, adc),  Instruction("NOP", 6, 1, 2, 0, nop),  Instruction("RRA", 3, 3, 7, 0, rra),
    Instruction("NOP", 2, 3, 4, 1, nop),  Instruction("ADC", 2, 3, 4, 1, adc),  Instruction("ROR", 2, 3, 7, 0, ror),  Instruction("RRA", 2, 3, 7, 0, rra),
    Instruction("NOP", 5, 2, 2, 0, nop),  Instruction("STA", 7, 2, 6, 0, sta),  Instruction("NOP", 5, 0, 2, 0, nop),  Instruction("SAX", 7, 2, 6, 0, sax),
    Instruction("STY", 11, 2, 3, 0, sty), Instruction("STA", 11, 2, 3, 0, sta), Instruction("STX", 11, 2, 3, 0, stx), Instruction("SAX", 11, 2, 3, 0, sax),
    Instruction("DEY", 6, 1, 2, 0, dey),  Instruction("NOP", 5, 0, 2, 0, nop),  Instruction("TXA", 6, 1, 2, 0, txa),  Instruction("XAA", 5, 0, 2, 0, xaa),
    Instruction("STY", 1, 3, 4, 0, sty),  Instruction("STA", 1, 3, 4, 0, sta),  Instruction("STX", 1, 3, 4, 0, stx),  Instruction("SAX", 1, 3, 4, 0, sax),
    Instruction("BCC", 10, 2, 2, 1, bcc), Instruction("STA", 9, 2, 6, 0, sta),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("AHX", 9, 0, 6, 0, ahx),
    Instruction("STY", 12, 2, 4, 0, sty), Instruction("STA", 12, 2, 4, 0, sta), Instruction("STX", 13, 2, 4, 0, stx), Instruction("SAX", 13, 2, 4, 0, sax),
    Instruction("TYA", 6, 1, 2, 0, tya),  Instruction("STA", 3, 3, 5, 0, sta),  Instruction("TXS", 6, 1, 2, 0, txs),  Instruction("TAS", 3, 0, 5, 0, tas),
    Instruction("SHY", 2, 0, 5, 0, shy),  Instruction("STA", 2, 3, 5, 0, sta),  Instruction("SHX", 3, 0, 5, 0, shx),  Instruction("AHX", 3, 0, 5, 0, ahx),
    Instruction("LDY", 5, 2, 2, 0, ldy),  Instruction("LDA", 7, 2, 6, 0, lda),  Instruction("LDX", 5, 2, 2, 0, ldx),  Instruction("LAX", 7, 2, 6, 0, lax),
    Instruction("LDY", 11, 2, 3, 0, ldy), Instruction("LDA", 11, 2, 3, 0, lda), Instruction("LDX", 11, 2, 3, 0, ldx), Instruction("LAX", 11, 2, 3, 0, lax),
    Instruction("TAY", 6, 1, 2, 0, tay),  Instruction("LDA", 5, 2, 2, 0, lda),  Instruction("TAX", 6, 1, 2, 0, tax),  Instruction("LAX", 5, 0, 2, 0, lax),
    Instruction("LDY", 1, 3, 4, 0, ldy),  Instruction("LDA", 1, 3, 4, 0, lda),  Instruction("LDX", 1, 3, 4, 0, ldx),  Instruction("LAX", 1, 3, 4, 0, lax),
    Instruction("BCS", 10, 2, 2, 1, bcs), Instruction("LDA", 9, 2, 5, 1, lda),  Instruction("KIL", 6, 2, 2, 0, kil),  Instruction("LAX", 9, 2, 5, 1, lax),
    Instruction("LDY", 12, 2, 4, 0, ldy), Instruction("LDA", 12, 2, 4, 0, lda), Instruction("LDX", 13, 2, 4, 0, ldx), Instruction("LAX", 13, 2, 4, 0, lax),
    Instruction("CLV", 6, 1, 2, 0, clv),  Instruction("LDA", 3, 3, 4, 1, lda),  Instruction("TSX", 6, 1, 2, 0, tsx),  Instruction("LAS", 3, 0, 4, 1, las),
    Instruction("LDY", 2, 3, 4, 1, ldy),  Instruction("LDA", 2, 3, 4, 1, lda),  Instruction("LDX", 3, 3, 4, 1, ldx),  Instruction("LAX", 3, 3, 4, 1, lax),
    Instruction("CPY", 5, 2, 2, 0, cpy),  Instruction("CMP", 7, 2, 6, 0, cmp),  Instruction("NOP", 5, 0, 2, 0, nop),  Instruction("DCP", 7, 2, 8, 0, dcp),
    Instruction("CPY", 11, 2, 3, 0, cpy), Instruction("CMP", 11, 2, 3, 0, cmp), Instruction("DEC", 11, 2, 5, 0, dec), Instruction("DCP", 11, 2, 5, 0, dcp),
    Instruction("INY", 6, 1, 2, 0, iny),  Instruction("CMP", 5, 2, 2, 0, cmp),  Instruction("DEX", 6, 1, 2, 0, dex),  Instruction("AXS", 5, 0, 2, 0, axs),
    Instruction("CPY", 1, 3, 4, 0, cpy),  Instruction("CMP", 1, 3, 4, 0, cmp),  Instruction("DEC", 1, 3, 6, 0, dec),  Instruction("DCP", 1, 3, 6, 0, dcp),
    Instruction("BNE", 10, 2, 2, 1, bne), Instruction("CMP", 9, 2, 5, 1, cmp),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("DCP", 9, 2, 8, 0, dcp),
    Instruction("NOP", 12, 2, 4, 0, nop), Instruction("CMP", 12, 2, 4, 0, cmp), Instruction("DEC", 12, 2, 6, 0, dec), Instruction("DCP", 12, 2, 6, 0, dcp),
    Instruction("CLD", 6, 1, 2, 0, cld),  Instruction("CMP", 3, 3, 4, 1, cmp),  Instruction("NOP", 6, 1, 2, 0, nop),  Instruction("DCP", 3, 3, 7, 0, dcp),
    Instruction("NOP", 2, 3, 4, 1, nop),  Instruction("CMP", 2, 3, 4, 1, cmp),  Instruction("DEC", 2, 3, 7, 0, dec),  Instruction("DCP", 2, 3, 7, 0, dcp),
    Instruction("CPX", 5, 2, 2, 0, cpx),  Instruction("SBC", 7, 2, 6, 0, sbc),  Instruction("NOP", 5, 0, 2, 0, nop),  Instruction("ISC", 7, 2, 8, 0, isc),
    Instruction("CPX", 11, 2, 3, 0, cpx), Instruction("SBC", 11, 2, 3, 0, sbc), Instruction("INC", 11, 2, 5, 0, inc), Instruction("ISC", 11, 2, 5, 0, isc),
    Instruction("INX", 6, 1, 2, 0, inx),  Instruction("SBC", 5, 2, 2, 0, sbc),  Instruction("NOP", 6, 1, 2, 0, nop),  Instruction("SBC", 5, 2, 2, 0, sbc),
    Instruction("CPX", 1, 3, 4, 0, cpx),  Instruction("SBC", 1, 3, 4, 0, sbc),  Instruction("INC", 1, 3, 6, 0, inc),  Instruction("ISC", 1, 3, 6, 0, isc),
    Instruction("BEQ", 10, 2, 2, 1, beq), Instruction("SBC", 9, 2, 5, 1, sbc),  Instruction("KIL", 6, 0, 2, 0, kil),  Instruction("ISC", 9, 2, 8, 0, isc),
    Instruction("NOP", 12, 2, 4, 0, nop), Instruction("SBC", 12, 2, 4, 0, sbc), Instruction("INC", 12, 2, 6, 0, inc), Instruction("ISC", 12, 2, 6, 0, isc),
    Instruction("SED", 6, 1, 2, 0, sed),  Instruction("SBC", 3, 3, 4, 1, sbc),  Instruction("NOP", 6, 1, 2, 0, nop),  Instruction("ISC", 3, 3, 7, 0, isc),
    Instruction("NOP", 2, 3, 4, 1, nop),  Instruction("SBC", 2, 3, 4, 1, sbc),  Instruction("INC", 2, 3, 7, 0, inc),  Instruction("ISC", 2, 3, 7, 0, isc)
  )

  private var cycles      = 0L                // number of cycles
  private var interrupt   = interrupts.Empty  // interrupt type to perform
  private var stall       = 0                 // number of cycles to stall
  private val registers   = Registers()

  def Reset():Unit = {
    memory.Write(0x4015, 0)
    memory.Write(0x4017, memory.Read(0x4017))
    //disable audio on reset
    registers.sp = registers.sp - 3
    registers.sp = registers.sp as uByte
    registers.i = 1
    registers.pc = initialProgramCounter
    registers.sp = initialStackPointer
  }
  // Step executes a single CPU instruction
  def Step():Int = {
    if (memory.ppu.DMAStall) {

      stall += 513

      if (this.cycles % 2 == 1) {
        stall += 1
      }
      memory.ppu.DMAStall = false
    }

    if(memory.apu.DMAStall) {
      stall += 4
      memory.apu.DMAStall = false
    }

    if (stall > 0) {
      stall = stall - 1
      return 1
    }

    val cycles = this.cycles

    interrupt match {
      case interrupts.NMI => nmi()
      case interrupts.IRQ => irq()
      case interrupts.Empty =>
    }

    interrupt = interrupts.Empty

    val opcode = memory.Read(registers.pc)
    val mode = instructions(opcode).mode

    val (address: Int, pageCrossed: Boolean) = getAddressWithPageCrossed(mode)

    registers.pc += instructions(opcode).size
    this.cycles += instructions(opcode).cycle
    if (pageCrossed) this.cycles += instructions(opcode).pageCycles

    instructions(opcode).op(address, registers.pc, mode)

    (this.cycles - cycles).toInt
  }

  @inline private def getAddressWithPageCrossed(mode: Byte): (Int, Boolean) = mode match {
      case addressModes.Absolute => read16(registers.pc + 1) -> false
      case addressModes.AbsoluteX => val addressOffsetX = (read16(registers.pc + 1) + registers.x) as uShort
        addressOffsetX -> pagesDiffer(addressOffsetX - registers.x, addressOffsetX)
      case addressModes.AbsoluteY => val addressOffsetY = (read16(registers.pc + 1) + registers.y) as uShort
        addressOffsetY -> pagesDiffer(addressOffsetY - registers.y, addressOffsetY)
      case addressModes.Accumulator => 0 -> false
      case addressModes.Immediate => registers.pc + 1 -> false
      case addressModes.Implied => 0 -> false
      case addressModes.IndexedIndirect => read16Bug((memory.Read(registers.pc + 1) + registers.x) as uByte) -> false
      case addressModes.Indirect => read16Bug(read16(registers.pc + 1)) -> false
      case addressModes.IndirectIndexed => val addressOffsetY = (read16Bug(memory.Read(registers.pc + 1)) + registers.y) as uShort
        addressOffsetY -> pagesDiffer(addressOffsetY - registers.y, addressOffsetY)
      case addressModes.Relative =>
        val offset = memory.Read(registers.pc + 1)
        if (offset < 0x80) registers.pc + 2 + offset -> false
        else registers.pc + 2 + offset - 0x100 -> false
      case addressModes.ZeroPage => memory.Read(registers.pc + 1) -> false
      case addressModes.ZeroPageX => ((memory.Read(registers.pc + 1) + registers.x) as uByte) -> false
      case addressModes.ZeroPageY => ((memory.Read(registers.pc + 1) + registers.y) as uByte) -> false
  }

  // Read16 reads two bytes using Read to return a double-word value
  private def read16(address: Int): Int = {
    val (lo, hi) = (memory.Read(address), memory.Read(address + 1))
    hi << 8 | lo
  }

  // read16bug emulates a 6502 bug that caused the low byte to wrap without incrementing the high byte
  private def read16Bug(address:Int): Int = {
    val a = address
    val b = (a & 0xFF00) | ((a + 1) as uByte)
    val (lo, hi) = (memory.Read(a), memory.Read(b))
    hi << 8 | lo
  }

  // SetFlags sets the processor status flags
  private def setFlags(flags:Int) = {
    registers.c = (flags >>> 0) & 1
    registers.z = (flags >>> 1) & 1
    registers.i = (flags >>> 2) & 1
    registers.d = (flags >>> 3) & 1
    registers.b = (flags >>> 4) & 1
    registers.u = (flags >>> 5) & 1
    registers.v = (flags >>> 6) & 1
    registers.n = (flags >>> 7) & 1
  }
  // NMI - Non-Maskable Interrupt
  private def nmi() {
    push16(registers.pc)
    php(0,0,0)
    registers.pc = read16(0xFFFA)
    registers.i = 1
    cycles += 7
  }

  // IRQ - IRQ Interrupt
  private def irq() {
    push16(registers.pc)
    php(0,0,0)
    registers.pc = read16(0xFFFE)
    registers.i = 1
    cycles += 7
  }
  // pull pops a byte from the stack
  private def pull:Int = {
    registers.sp = (registers.sp + 1) as uByte
    memory.Read(0x100 | registers.sp)
  }
  // pull16 pops two bytes from the stack
  private def pull16:Int = {
    val (lo, hi) = (pull, pull)
    (hi << 8) | lo
  }
  // push pushes a byte onto the stack
  private def push(value:Int) = {
    memory.Write(0x100 | registers.sp, value)
    registers.sp = (registers.sp - 1) as uByte
  }
  // push16 pushes two bytes onto the stack
  private def push16(value:Int) = {
    val (hi, lo) = (value >>> 8, value as uByte)
    push(hi)
    push(lo)
  }

  // pagesDiffer returns true if the two addresses reference different pages
  private def pagesDiffer(a:Int, b:Int):Boolean = (a & 0xFF00) != (b & 0xFF00)

  // Flags returns the processor status flags
  private def flags:Int = {
    var flags = 0
    flags |= registers.c << 0
    flags |= registers.z << 1
    flags |= registers.i << 2
    flags |= registers.d << 3
    flags |= registers.b << 4
    flags |= registers.u << 5
    flags |= registers.v << 6
    flags |= registers.n << 7
    flags
  }

  private def setZ(x:Int):Unit = if (x == 0) registers.z = 1 else registers.z = 0
  // setN sets the negative flag if the argument is negative (high bit is set)
  private def setN(x:Int):Unit = if ((x & 0x80) != 0) registers.n = 1 else registers.n = 0
  // setZN sets the zero flag and the negative flag
  private def setZN(x:Int):Unit = { setZ(x); setN(x) }
  // triggerNMI causes a non-maskable interrupt to occur on the next cycle
  def triggerNMI = interrupt = interrupts.NMI
  // triggerIRQ causes an IRQ interrupt to occur on the next cycle
  def triggerIRQ = if (registers.i == 0) interrupt = interrupts.IRQ

  // Adds a cycle for taking a branch and adds another cycle if the branch jumps to a new page
  private val addBranchCycles = (f: () => Boolean) => (address:Int, pc:Int, mode:Byte) => {
    if(f()) {
      registers.pc = address
      cycles += 1
      if (pagesDiffer(pc, address)) cycles += 1
    }
  }

  private val compare: (Int, Int) => Unit = { (a, b) =>
    setZN(a - b)
    if (a >= b) registers.c = 1 else registers.c = 0
  }

  // ADC - Add with Carry
  private lazy val adc:Operation = { (address:Int, pc:Int, mode:Byte) =>
    val (a, b, c) = (registers.a, memory.Read(address), registers.c)
    registers.a = (a + b + c) as uByte
    setZN(registers.a)

    if ((a + b + c) > 0xFF) registers.c = 1 else registers.c = 0
    if (((a ^ b) & 0x80) == 0 && ((a ^ registers.a) & 0x80) != 0) registers.v = 1 else registers.v = 0
  }
  // AND - Logical AND
  private lazy val and:Operation = { (address:Int, pc:Int, mode:Byte) =>
    registers.a = registers.a & memory.Read(address)
    setZN(registers.a)
  }
  // ASL - Arithmetic Shift Left
  private lazy val asl:Operation = (address:Int, pc:Int, mode:Byte) => mode match {
      case addressModes.Accumulator =>
        registers.c = (registers.a >>> 7) & 1
        registers.a = (registers.a << 1) as uByte
        setZN(registers.a)
      case other =>
        val readValue = memory.Read(address)
        registers.c = (readValue >>> 7) & 1
        val writeValue = (readValue << 1) as uByte
        memory.Write(address, writeValue)
        setZN(writeValue)
  }
  // BCC - Branch if Carry Clear
  private lazy val bcc:Operation = (address:Int, pc:Int, mode:Byte) => addBranchCycles{ () => registers.c == 0 }(address, pc, mode)
  // BCS - Branch if Carry Set
  private lazy val bcs:Operation = (address:Int, pc:Int, mode:Byte) => addBranchCycles{ () => registers.c != 0 }(address, pc, mode)
  // BEQ - Branch if Equal
  private lazy val beq:Operation = (address:Int, pc:Int, mode:Byte) => addBranchCycles{ () => registers.z != 0 }(address, pc, mode)
  // BIT - Bit Test
  private lazy val bit:Operation = { (address:Int, pc:Int, mode:Byte) =>
    val value = memory.Read(address)
    registers.v = (value >>> 6) & 1
    setZ(value & registers.a)
    setN(value)
  }
  // BMI - Branch if Minus
  private lazy val bmi:Operation = (address:Int, pc:Int, mode:Byte) => addBranchCycles{ () => registers.n != 0 }(address, pc, mode)
  // BNE - Branch if Not Equal
  private lazy val bne:Operation = (address:Int, pc:Int, mode:Byte) => addBranchCycles{ () => registers.z == 0 }(address, pc, mode)
  // BPL - Branch if Positive
  private lazy val bpl:Operation = (address:Int, pc:Int, mode:Byte) => addBranchCycles{ () => registers.n == 0}(address, pc, mode)
  // BRK - Force Interrupt
  private lazy val brk:Operation = { (address:Int, pc:Int, mode:Byte) =>
    push16(registers.pc + 1)
    php(0,0,0)
    sei(address, pc, mode)
    registers.pc = read16(0xFFFE)
  }
  // BVC - Branch if Overflow Clear
  private lazy val bvc:Operation = (address:Int, pc:Int, mode:Byte) => addBranchCycles{ () => registers.v == 0}(address, pc, mode)
  // BVS - Branch if Overflow Set
  private lazy val bvs:Operation = (address:Int, pc:Int, mode:Byte) => addBranchCycles{ () => registers.v != 0 }(address, pc, mode)
  // CLC - Clear Carry Flag
  private lazy val clc:Operation = (address:Int, pc:Int, mode:Byte) => registers.c = 0
  // CLD - Clear Decimal Mode
  private lazy val cld:Operation = (address:Int, pc:Int, mode:Byte) => registers.d = 0
  // CLI - Clear Interrupt Disable
  private lazy val cli:Operation = (address:Int, pc:Int, mode:Byte) => registers.i = 0
  // CLV - Clear Overflow Flag
  private lazy val clv:Operation = (address:Int, pc:Int, mode:Byte) => registers.v = 0
  // CMP - Compare
  private lazy val cmp:Operation = (address:Int, pc:Int, mode:Byte) => compare(registers.a, memory.Read(address))
  // CPX - Compare X Register
  private lazy val cpx:Operation = (address:Int, pc:Int, mode:Byte) => compare(registers.x, memory.Read(address))
  // CPY - Compare Y Register
  private lazy val cpy:Operation = (address:Int, pc:Int, mode:Byte) => compare(registers.y, memory.Read(address))
  // DEC - Decrement Memory
  private lazy val dec:Operation = (address:Int, pc:Int, mode:Byte) => {
    val value = (memory.Read(address) - 1) as uByte
    memory.Write(address, value)
    setZN(value)
  }
  // DEX - Decrement X Register
  private lazy val dex:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.x = (registers.x - 1) as uByte
    setZN(registers.x)
  }
  // DEY - Decrement Y Register
  private lazy val dey:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.y = (registers.y - 1) as uByte
    setZN(registers.y)
  }
  // EOR - Exclusive OR
  private lazy val eor:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.a = (registers.a ^ memory.Read(address)) as uByte
    setZN(registers.a)
  }
  // INC - Increment Memory
  private lazy val inc:Operation = (address:Int, pc:Int, mode:Byte) => {
    val value = (memory.Read(address) + 1) as uByte
    memory.Write(address, value)
    setZN(value)
  }
  // INX - Increment X Register
  private lazy val inx:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.x = (registers.x + 1) as uByte
    setZN(registers.x)
  }
  // INY - Increment Y Register
  private lazy val iny:Operation = (address:Int, pc:Int, mode:Byte) =>  {
    registers.y = (registers.y + 1) as uByte
    setZN(registers.y)
  }
  // JMP - Jump
  private lazy val jmp:Operation = (address:Int, pc:Int, mode:Byte) => registers.pc = address
  // JSR - Jump to Subroutine
  private lazy val jsr:Operation = (address:Int, pc:Int, mode:Byte) => {
    push16((registers.pc - 1) as uShort)
    registers.pc = address
  }
  // LDA - Load Accumulator
  private lazy val lda:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.a = memory.Read(address)
    setZN(registers.a)
  }
  // LDX - Load X Register
  private lazy val ldx:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.x = memory.Read(address)
    setZN(registers.x)
  }
  // LDY - Load Y Register
  private lazy val ldy:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.y = memory.Read(address)
    setZN(registers.y)
  }
  // LSR - Logical Shift Right
  private lazy val lsr:Operation = (address:Int, pc:Int, mode:Byte) => {
    if (mode == addressModes.Accumulator) {
      registers.c = registers.a & 1
      registers.a = registers.a >>> 1
      setZN(registers.a)
    } else {
      var value = memory.Read(address)
      registers.c = value & 1
      value = value >>> 1
      memory.Write(address, value)
      setZN(value)
    }
  }
  // NOP - No Operation
  private lazy val nop:Operation = (address:Int, pc:Int, mode:Byte) => {}
  // ORA - Logical Inclusive OR
  private lazy val ora:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.a = registers.a | memory.Read(address)
    setZN(registers.a)
  }
  // PHA - Push Accumulator
  private lazy val pha:Operation = (address:Int, pc:Int, mode:Byte) => push(registers.a)
  // PHP - Push Processor Status
  private lazy val php:Operation = (address:Int, pc:Int, mode:Byte) => push(flags | 0x10)
  // PLA - Pull Accumulator
  private lazy val pla:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.a = pull
    setZN(registers.a)
  }
  // PLP - Pull Processor Status
  private lazy val plp:Operation = (address:Int, pc:Int, mode:Byte) => setFlags(pull & 0xEF | 0x20)
  // ROL - Rotate Left
  private lazy val rol:Operation = (address:Int, pc:Int, mode:Byte) => {
    val c = registers.c
    if (mode == addressModes.Accumulator) {
      registers.c = (registers.a >>> 7) & 1
      registers.a = ((registers.a << 1) as uByte) | c
      setZN(registers.a)
    } else {
      var value = memory.Read(address)
      registers.c = (value >>> 7) & 1
      value = ((value << 1) as uByte)| c
      memory.Write(address, value)
      setZN(value)
    }
  }
  // ROR - Rotate Right
  private lazy val ror:Operation = (address:Int, pc:Int, mode:Byte) => {
    val c = registers.c
    if (mode == addressModes.Accumulator) {
      registers.c = registers.a & 1
      registers.a = (registers.a >>> 1) | ((c << 7) as uByte)
      setZN(registers.a)
    } else {
      var value = memory.Read(address)
      registers.c = value & 1
      value = (value >> 1) | ((c << 7) as uByte)
      memory.Write(address, value)
      setZN(value)
    }
  }
  // RTI - Return from Interrupt
  private lazy val rti:Operation = (address:Int, pc:Int, mode:Byte) => {
    setFlags(pull & 0xEF | 0x20)
    registers.pc = pull16
  }
  // RTS - Return from Subroutine
  private lazy val rts:Operation = (address:Int, pc:Int, mode:Byte) => registers.pc = (pull16 + 1) as uShort

  // SBC - Subtract with Carry
  private lazy val sbc:Operation = (address:Int, pc:Int, mode:Byte) => {
    val (a, b, c) = (registers.a, memory.Read(address), registers.c)
    val value = (a - b - (1 - c)) as uByte
    registers.a = value
    setZN(registers.a)
    if (a - b - (1 - c) >= 0) registers.c = 1 else registers.c = 0
    if ((((a^b) & 0x80) != 0) && (((a ^ registers.a) & 0x80) != 0)) registers.v = 1 else registers.v = 0
  }
  // SEC - Set Carry Flag
  private lazy val sec:Operation = (address:Int, pc:Int, mode:Byte) => registers.c = 1
  // SED - Set Decimal Flag
  private lazy val sed:Operation = (address:Int, pc:Int, mode:Byte) => registers.d = 1
  // SEI - Set Interrupt Disable
  private lazy val sei:Operation = (address:Int, pc:Int, mode:Byte) => registers.i = 1
  // STA - Store Accumulator
  private lazy val sta:Operation = (address:Int, pc:Int, mode:Byte) => memory.Write(address, registers.a)
  // STX - Store X Register
  private lazy val stx:Operation = (address:Int, pc:Int, mode:Byte) => memory.Write(address, registers.x)
  // STY - Store Y Register
  private lazy val sty:Operation = (address:Int, pc:Int, mode:Byte) => memory.Write(address, registers.y)
  // TAX - Transfer Accumulator to X
  private lazy val tax:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.x = registers.a
    setZN(registers.x)
  }
  // TAY - Transfer Accumulator to Y
  private lazy val tay:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.y = registers.a
    setZN(registers.y)
  }
  // TSX - Transfer Stack Pointer to X
  private lazy val tsx:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.x = registers.sp
    setZN(registers.x)
  }
  // TXA - Transfer X to Accumulator
  private lazy val txa:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.a = registers.x
    setZN(registers.a)
  }
  // TXS - Transfer X to Stack Pointer
  private lazy val txs:Operation = (address:Int, pc:Int, mode:Byte) => registers.sp = registers.x
  // TYA - Transfer Y to Accumulator
  private lazy val tya:Operation = (address:Int, pc:Int, mode:Byte) => {
    registers.a = registers.y
    setZN(registers.a)
  }

  // illegal opcodes below
  private lazy val lax:Operation = (address:Int, pc:Int, mode:Byte) => {
    val value = memory.Read(address)
    registers.a = value
    registers.x = value
    setZN(value)
  }

  private lazy val sax:Operation = (address:Int, pc:Int, mode:Byte) => {
    memory.Write(address, registers.a & registers.x)
  }

  private lazy val dcp:Operation = (address:Int, pc:Int, mode:Byte) => {
    dec(address, pc, mode)
    cmp(address, pc, mode)
  }

  private lazy val isc:Operation = (address:Int, pc:Int, mode:Byte) => {
    inc(address, pc, mode)
    sbc(address, pc, mode)
  }

  private lazy val slo:Operation = (address:Int, pc:Int, mode:Byte) => {
    asl(address, pc, mode)
    ora(address, pc, mode)
  }

  private lazy val rla:Operation = (address:Int, pc:Int, mode:Byte) => {
    rol(address, pc, mode)
    and(address, pc, mode)
  }

  private lazy val sre:Operation = (address:Int, pc:Int, mode:Byte) => {
    lsr(address, pc, mode)
    eor(address, pc, mode)
  }

  private lazy val rra:Operation = (address:Int, pc:Int, mode:Byte) => {
    ror(address, pc, mode)
    adc(address, pc, mode)
  }

  private lazy val ahx, alr, anc, arr, axs, kil, las, shx, shy, tas, xaa:Operation = (address:Int, pc:Int, mode:Byte) => {}

  override def toString = {
    val opcode  = memory.Read(registers.pc)
    val name    = instructions(opcode).name
    val mode    = instructions(opcode).mode
    val (address, pageCrossed) = getAddressWithPageCrossed(mode)

    def formatHex(i: Int, size: Int = 2):String = Integer.toHexString(i).toUpperCase.reverse.padTo(size, 0).reverse.mkString.grouped(2).reduce{
      (a, b) => if (a == "00" && size == 2) b.reverse.padTo(2, "0").reverse.mkString else s"${a.reverse.padTo(2, "0").reverse.mkString}${b.reverse.padTo(2, "0").reverse.mkString}"
    }

    val pcFormat = Integer.toHexString(registers.pc).toUpperCase.reverse.padTo(4, "0").reverse.mkString
    val rawInstructionFormat =
      if (mode == addressModes.Implied || mode == addressModes.Accumulator) f"${formatHex(opcode)}%-9s"
      else if (mode == addressModes.Immediate || mode == addressModes.ZeroPage) f"${formatHex(opcode)}%-2s ${formatHex(address, 4).grouped(2).reduce((a, b) => s"$b")}%-6s"
      else if(mode == addressModes.Indirect) f"${formatHex(opcode)} ${formatHex(read16(registers.pc + 1), 4).grouped(2).reduce((a, b) => s"$b $a")}%-6s"
      else if (mode == addressModes.Relative || mode == addressModes.IndexedIndirect || mode == addressModes.IndirectIndexed) f"${formatHex(opcode)} ${formatHex(memory.Read(registers.pc + 1))}%-6s"
      else f"${formatHex(opcode)}%-2s ${formatHex(address, 4).grouped(2).reduce((a, b) => s"$b $a")}%-6s"
    val instructionParameterFormat =
      mode match{
        case addressModes.Immediate => f"#$$${formatHex(address)}%-25s"
        case addressModes.ZeroPage => f"$$${formatHex(address)} = ${formatHex(memory.Read(address))}%-21s"
        case addressModes.ZeroPageX =>
          val addr = memory.Read(registers.pc + 1)
          val offset = (addr  + registers.x) as uByte
          f"$$${formatHex(addr)},X @ ${formatHex(offset)} = ${formatHex(memory.Read(address))}%-14s"
        case addressModes.Accumulator => "A".padTo(27, " ").mkString
        case addressModes.Implied => " ".padTo(27, " ").mkString
        case addressModes.IndexedIndirect =>
          val address = memory.Read(registers.pc + 1)
          val offset = (address + registers.x) as uByte
          f"($$${formatHex(address)},X) @ ${formatHex(offset)} = ${formatHex(address, 4)} = ${formatHex(memory.Read(address))}%-5s"
        case addressModes.IndirectIndexed =>
          val address = memory.Read(registers.pc + 1)
          val offset = read16Bug(address)
          val offsetY = (offset  + registers.y) as uShort
          f"($$${formatHex(address)}),Y = ${formatHex(offset, 4)} @ ${formatHex(offsetY, 4)} = ${formatHex(memory.Read(address))}%-3s"
        case addressModes.Absolute => f"$$${formatHex(address, 4)}%-26s"
        case addressModes.Indirect => f"($$${formatHex(read16(registers.pc + 1), 4)}) = ${formatHex(memory.Read(address), 4)}%-17s"
        case _ => f"$$${formatHex(address, 4)}%-26s"
      }
    val aFormat = f"A:${formatHex(registers.a)}%-2s"
    val xFormat = f"X:${formatHex(registers.x)}%-2s"
    val yFormat = f"Y:${formatHex(registers.y)}%-2s"
    val pFormat = f"P:${formatHex(flags)}%-2s"
    val spFormat = f"SP:${formatHex(registers.sp)}%-2s"
    val cycFormat = f"CYC:${(cycles * 3) % 341}%3d"
    val slFormat = f"SL:${memory.ppu.ScanLine}%-3d"

    f"$pcFormat  $rawInstructionFormat ${if(pageCrossed) "*" else ""}$name $instructionParameterFormat $aFormat $xFormat $yFormat $pFormat $spFormat $cycFormat $slFormat"
  }

  private def initialiseMemory() = {
    // puts RAM in NES power-on state
    for (i <- 0 to 0x800) memory.Write(i, 0xFF)

    memory.Write(0x0008, 0xF7)
    memory.Write(0x0009, 0xEF)
    memory.Write(0x000A, 0xDF)
    memory.Write(0x000F, 0xBF)

    for (i <- 0x4000 to 0x400F) memory.Write(i, 0x00)

    memory.Write(0x4015, 0x00)
    memory.Write(0x4017, 0x00)
  }

  initialiseMemory()
}

object CPU {
  val frequency = 1789773
  // interrupt types
  private object interrupts extends Enumeration {
    type Interrupt = Value
    val Empty = Value
    val NMI   = Value
    val IRQ   = Value
  }

  // address modes
  private object addressModes {
    val Absolute: Byte        = 1
    val AbsoluteX: Byte       = 2
    val AbsoluteY: Byte       = 3
    val Accumulator: Byte     = 4
    val Immediate: Byte       = 5
    val Implied: Byte         = 6
    val IndexedIndirect: Byte = 7
    val Indirect: Byte        = 8
    val IndirectIndexed: Byte = 9
    val Relative: Byte        = 10
    val ZeroPage: Byte        = 11
    val ZeroPageX: Byte       = 12
    val ZeroPageY: Byte       = 13
  }
  // Reset resets the CPU to its initial power-up state
  def apply(ram:Array[Int], ppu:PPU, apu:APU, controller1:Controller, controller2:Controller, mapper:Mapper): CPU = {
    val cpu = new CPU(CPUMemory(ram, ppu, apu, controller1, controller2, mapper))
    cpu.setFlags(0x24)
    cpu
  }
}
