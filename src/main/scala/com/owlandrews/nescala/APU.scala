package com.owlandrews.nescala

case class APU(channel: (Int) => Unit) {

  var DMAStall = false
  private var cycle = 0L
  private val frameCounterRate = CPU.frequency / 240
  private val sampleRate = CPU.frequency / 44100.0
  private val filterChain = FilterChain(44100)
  private case class Length(var enabled:Boolean = false, var value:Int = 0,
                            table:Array[Int] = Array(10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
                                          12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30))
  private case class Timer(var period:Int = 0, var value:Int = 0)
  private case class Duty (var mode:Int = 0, var value:Int = 0)
  private object Duty {
    val table = Array(
      Array(0, 1, 0, 0, 0, 0, 0, 0),
      Array(0, 1, 1, 0, 0, 0, 0, 0),
      Array(0, 1, 1, 1, 1, 0, 0, 0),
      Array(1, 0, 0, 1, 1, 1, 1, 1)
    )
  }
  private case class Envelope (var enabled:Boolean = false, var loop :Boolean = false, var start: Boolean = false,
                               var period:Int = 0, var value:Int = 0, var volume:Int = 0)

  private val tndTable = for (i <- 0 to 202) yield (163.67 / (24329.0 / i + 100)  * 49151).toInt
  private object Pulse {
    val table = for (i <- 0 to 30) yield (95.52 / (8128.0 / i + 100) * 49151).toInt
  }
  private case class Pulse(channel:Int) {
    case class Sweep(var reload:Boolean = false, var enabled:Boolean = false, var negate:Boolean = false,
                     var shift: Int = 0, var period:Int = 0,var value:Int = 0)

    var enabled        = false
    val length         = Length()
    val timer          = Timer()
    val duty           = Duty()
    val sweep          = Sweep()
    val envelope       = Envelope()
    var constantVolume = 0

    def writeControl(value:Int) = {
      duty.mode = (value >>> 6) & 3
      length.enabled = ((value >>> 5) & 1) == 0
      envelope.loop = ((value >>> 5) & 1) == 1
      envelope.enabled = ((value >>> 4) & 1) == 0
      envelope.period = value & 15
      constantVolume = value & 15
      envelope.start = true
    }

    def writeSweep(value:Int) = {
      sweep.enabled = ((value >>> 7) & 1) == 1
      sweep.period  = (((value >>> 4) & 7) + 1) as uByte
      sweep.negate  = ((value >>> 3) & 1) == 1
      sweep.shift   = value & 7
      sweep.reload  = true
    }

    def writeTimerLow(value:Int) = timer.period = (timer.period & 0xFF00) | (value as uShort)

    def writeTimerHigh(value:Int) = {
      if(enabled) length.value = length.table(value >>> 3)
      timer.period = (timer.period & 0x00FF) | (((value & 7) as uShort) << 8)
      envelope.start = true
      duty.value = 0
    }

    def stepTimer() = if (timer.value == 0) {
        timer.value = timer.period
		    duty.value = ((duty.value + 1) as uByte) % 8
	  } else timer.value = (timer.value - 1) as uShort

    def stepLength() = if (length.enabled && length.value > 0) length.value = (length.value - 1) as uByte

    def stepEnvelope() = {
      if (envelope.start) {
        envelope.volume = 15
        envelope.value = envelope.period as uByte
        envelope.start = false
      }
      else if (envelope.value > 0) envelope.value = (envelope.value - 1) as uByte
      else {
        if (envelope.volume > 0) envelope.volume = (envelope.volume - 1) as uByte
        else if (envelope.loop) envelope.volume = 15

        envelope.value = envelope.period
      }
    }

    def stepSweep() = {
      if (sweep.reload) {
        if (sweep.enabled && sweep.value == 0) doSweep()
        sweep.value = sweep.period
        sweep.reload = false
      }
      else if (sweep.value > 0) sweep.value = (sweep.value - 1) as uByte
      else {
        if (sweep.enabled) doSweep()
        sweep.value = sweep.period
      }
    }

    def doSweep() = {
	    val delta = (timer.period >>> sweep.shift) as uByte
      timer.period = if (sweep.negate) {
        if (channel == 1) (timer.period - delta - 1) as uShort
        else  (timer.period - delta) as uShort
	    } else  (timer.period + delta) as uShort
    }

    def output():Int = {
      if (!enabled) return 0
      if (length.value == 0) return 0
      if (Duty.table(duty.mode)(duty.value) == 0) return 0
      if (timer.period < 8 || timer.period > 0x7FF) return 0
      // if (!sweepNegate && timerPeriod + (timerPeriod >>> sweepShift) > 0x7FF) return 0

      if (envelope.enabled) envelope.volume
      else constantVolume
    }
  }

  private case class Triangle(var enabled:Boolean = false) {
    val length = Length()
    val timer = Timer()
    val duty = Duty()
    var counterPeriod = 0
    var counterValue = 0
    var counterReload = false
    val table = Array(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,
                      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

    def writeControl(value:Int) = {
      length.enabled = ((value >>> 7) & 1) == 0
      counterPeriod = value & 0x7F
    }

    def writeTimerLow(value:Int) = timer.period = (timer.period & 0xFF00) | (value as uShort)

    def writeTimerHigh(value:Int) = {
      if(enabled) length.value = length.table(value >>> 3)
      timer.period = (timer.period & 0x00FF) | (((value & 7) as uShort) << 8)
      timer.value = timer.period
      counterReload = true
    }

    def stepTimer() = if (timer.value == 0) {
        timer.value = timer.period
		    if (length.value > 0 && counterValue > 0) duty.value = ((duty.value + 1) as uByte) % 32
	  } else timer.value = (timer.value - 1) as uShort

    def stepLength() = if (length.enabled && length.value > 0) length.value = (length.value - 1) as uByte

    def stepCounter() = {
	    if (counterReload) counterValue = counterPeriod else if (counterValue > 0) counterValue = (counterValue - 1) as uByte
	    if (length.enabled) counterReload = false
    }

    def output():Int = {
      if (!enabled) return 0
      if (length.value == 0) return 0
      if (counterValue == 0) return 0
      triangle.table(duty.value)
    }
  }

  private case class Noise(var enabled:Boolean = false) {
    var mode = false
    var shiftRegister = 1
    val length = Length()
    val timer = Timer()
    val envelope = Envelope()
    var constantVolume = 0
    private val table = Array(4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068)

    def writeControl(value:Int) = {
      length.enabled = ((value >>> 5) & 1) == 0
      envelope.loop = ((value >>> 5) & 1) == 1
      envelope.enabled = ((value >>> 4) & 1) == 0
      envelope.period = value & 15
      constantVolume = (value & 15) as uByte
      envelope.start = true
    }

    def writePeriod(value:Int) = {
      mode = (value & 0x80) == 0x80
      timer.period = table(value & 0x0F)
    }

    def writeLength(value:Int) = {
      if(enabled) length.value = length.table(value >>> 3)
      envelope.start = true
    }

    def stepTimer() = {
      if (timer.value == 0) {
        timer.value = timer.period
        val shift = if (mode) 6 else 1
        val b1 = shiftRegister & 1
        val b2 = (shiftRegister >>> shift) & 1
        shiftRegister = (shiftRegister >>> 1) as uShort
        shiftRegister = shiftRegister | (((b1 ^ b2) << 14) as uShort)
      } else timer.value = (timer.value - 1) as uShort
    }

    def stepEnvelope() = {
	    if (envelope.start) {
		    envelope.volume = 15
		    envelope.value = envelope.period as uByte
		    envelope.start = false
	    }
      else if (envelope.value > 0) envelope.value = (envelope.value - 1) as uByte
      else {
		    if (envelope.volume > 0) envelope.volume = (envelope.volume - 1) as uByte
        else if (envelope.loop) envelope.volume = 15

        envelope.value = envelope.period
	    }
    }

    def stepLength() = if (length.enabled && length.value > 0) length.value = (length.value - 1) as uByte

    def output():Int = {
      if (!enabled) return 0
      if (length.value == 0) return 0
      if ((shiftRegister&1) == 1) return 0
      if (envelope.enabled) envelope.volume
      else constantVolume
    }
  }

  // DMC

  private[APU] case class DMC(var enabled:Boolean, var value:Int, var sampleAddress:Int, var sampleLength:Int, var currentAddress:Int, var currentLength:Int,
                              var shiftRegister:Int, var bitCount:Int, var tickPeriod:Int, var tickValue:Int, var loop:Boolean, var irq:Boolean){
    val timer = Timer()
    private val table = Array[Int](214, 190, 170, 160, 143, 127, 113, 107, 95, 80, 71, 64, 53, 42, 36, 27)

    def writeValue(value:Int) = this.value = (value & 0x7F) as uByte

    def writeControl(value:Int) = {
	    irq = (value & 0x80) == 0x80
	    loop = (value & 0x40) == 0x40
	    tickPeriod = table(value & 0x0F)
    }

    // Sample address = %11AAAAAA.AA000000
    def writeAddress(value:Int) = sampleAddress = 0xC000 | ((value << 6) as uShort)

    def writeLength(value:Int) = {
	    // Sample length = %0000LLLL.LLLL0001
	    sampleLength = ((value << 4) as uShort) | 1
    }

    def restart() = {
	    currentAddress = sampleAddress
	    currentLength = sampleLength
    }

    def stepTimer(dmaRead: Int => Int):Unit = {
	    if (!enabled) return
	    stepReader(dmaRead)
      if (tickValue == 0) {
        tickValue = tickPeriod
        stepShifter()
      } else tickValue = (tickValue - 1) as uByte
    }


    def stepReader(dmaRead: Int => Int) = {
	    if (currentLength > 0 && bitCount == 0) {
        DMAStall = true
		    shiftRegister = dmaRead(currentAddress)
		    bitCount = 8
		    currentAddress = (currentAddress + 1) as uShort

        if (currentAddress == 0) currentAddress = 0x8000

        currentLength = (currentAddress - 1) as uShort

        if (currentLength == 0 && loop) restart()
	    }
    }

    def stepShifter():Unit = {
	    if (bitCount == 0) return

      if ((shiftRegister & 1) == 1) {
		    if (value <= 125) value = (value + 2) as uByte
	    } else {
        if (value >= 2) value = (value - 2) as uByte
	    }
      shiftRegister >>>= 1
      bitCount = (bitCount - 1) as uByte
    }

    def output(): Int = value
  }

  private val dmc                 = DMC(enabled = false, 0, 0, 0, 0, 0, 0, 0, 0, 0, loop = false, irq = false)
  private val pulse1              = new Pulse(1)
  private val pulse2              = new Pulse(2)
  private val triangle            = new Triangle
  private val noise               = new Noise
  private var framePeriod         = 0
  private var frameValue          = 0
  private var frameIRQ            = false
  private var frameIRQActive      = false

  def ReadRegister(address: Int): Int = address match {
   case 0x4015  => readStatus
   case default => 0
  }

  def WriteRegister(address: Int, value: Int) = address match {
    case 0x4000          => pulse1.writeControl(value)
    case 0x4001          => pulse1.writeSweep(value)
    case 0x4002          => pulse1.writeTimerLow(value)
    case 0x4003          => pulse1.writeTimerHigh(value)
    case 0x4004          => pulse2.writeControl(value)
    case 0x4005          => pulse2.writeSweep(value)
    case 0x4006          => pulse2.writeTimerLow(value)
    case 0x4007          => pulse2.writeTimerHigh(value)
    case 0x4008          => triangle.writeControl(value)
    case 0x4009 | 0x4010 => dmc.writeControl(value)
    case 0x4011          => dmc.writeValue(value)
    case 0x4012          => dmc.writeAddress(value)
    case 0x4013          => dmc.writeLength(value)
    case 0x400A          => triangle.writeTimerLow(value)
    case 0x400B          => triangle.writeTimerHigh(value)
    case 0x400C          => noise.writeControl(value)
    case 0x400D | 0x400E => noise.writePeriod(value)
    case 0x400F          => noise.writeLength(value)
    case 0x4015          => writeControl(value)
    case 0x4017          => writeFrameCounter(value)
  }

  def Step (dmaRead: Int => Int, fireIRQHandler: => Unit) = {
    val cycle1 = cycle
    cycle = (cycle + 1) as uLong
    val cycle2 = cycle

    stepTimer(dmaRead)

    val f1 = (cycle1 / frameCounterRate).toInt
    val f2 = (cycle2 / frameCounterRate).toInt

    if (f1 != f2) stepFrameCounter(fireIRQHandler)

    val s1 = (cycle1 / sampleRate).toInt
    val s2 = (cycle2 / sampleRate).toInt

    if (s1 != s2) sendSample()
  }

  def sendSample() = channel(filterChain.Step(output()))

  def output(): Int = {
    val p1 = pulse1.output()
    val p2 = pulse2.output()
    val t = triangle.output()
    val n = noise.output()
    val d = dmc.output()
    val pulseOut = Pulse.table(p1 + p2)
    val tndOut = tndTable(3 * t + 2 * n + d)
    pulseOut + tndOut
  }

  def readStatus = {
    var result = 0
    if (pulse1.length.value > 0)    result |= 1
    if (pulse2.length.value > 0)    result |= 2
    if (triangle.length.value > 0)  result |= 4
    if (noise.length.value > 0)     result |= 8
    if (dmc.currentLength > 0)      result |= 16
    if (frameIRQ)                   result |= 64
    if (dmc.irq)                    result |= 128
    frameIRQ = false
    result
  }

  private def writeControl(value:Int) = {
    pulse1.enabled    = (value & 1)   == 1
    pulse2.enabled    = (value & 2)   == 2
    triangle.enabled  = (value & 4)   == 4
    noise.enabled     = (value & 8)   == 8
    dmc.enabled       = (value & 16)  == 16
    if (!pulse1.enabled)    pulse1.length.value   = 0
    if (!pulse2.enabled)    pulse2.length.value   = 0
    if (!triangle.enabled)  triangle.length.value = 0
    if (!noise.enabled)     noise.length.value    = 0
    if (!dmc.enabled)       dmc.currentLength     = 0
  }

  private def writeFrameCounter(value:Int) = {
    framePeriod    = (4 + ((value >> 7) & 1)) as uByte
    frameIRQActive = (((value >>> 6) & 1) as uByte) == 0
    //frameValue     = 0

    if (framePeriod == 5) {
     	stepEnvelope()
     	stepSweep()
     	stepLength()
    }

    if(!frameIRQActive) frameIRQ = false
  }

  private def stepTimer(dmaRead: Int => Int) = {
    if (cycle % 2 == 0) {
      pulse1.stepTimer()
      pulse2.stepTimer()
      noise.stepTimer()
      dmc.stepTimer(dmaRead)
    }
    triangle.stepTimer()
  }

    // mode 0:    mode 1:       function
    // ---------  -----------  -----------------------------
    //  - - - f    - - - - -    IRQ (if bit 6 is clear)
    //  - l - l    l - l - -    Length counter and sweep
    //  e e e e    e e e e -    Envelope and linear counter
   def stepFrameCounter(fireIRQHandler: => Unit) = framePeriod match {
      case 4 =>
        frameValue = ((frameValue + 1) as uByte) % 4
        frameValue match {
          case 0 | 2 =>
            stepEnvelope()
          case 1 =>
            stepEnvelope()
            stepSweep()
            stepLength()
          case 3 =>
            stepEnvelope()
            stepSweep()
            stepLength()
            fireIRQ(fireIRQHandler)
         }
      case 5 =>
        frameValue = ((frameValue + 1) as uByte) % 5
        frameValue match {
          case 1 | 3 =>
            stepEnvelope()
          case 0 | 2 =>
            stepEnvelope()
            stepSweep()
            stepLength()
          case default =>
      }
      case default =>
   }

  def stepEnvelope() = {
	  pulse1.stepEnvelope()
	  pulse2.stepEnvelope()
	  triangle.stepCounter()
	  noise.stepEnvelope()
  }

  def stepSweep() = {
	  pulse1.stepSweep()
    pulse2.stepSweep()
  }

  def stepLength() = {
	  pulse1.stepLength()
	  pulse2.stepLength()
	  triangle.stepLength()
	  noise.stepLength()
  }

  def fireIRQ(fireIRQHandler: => Unit) = if (frameIRQActive && !frameIRQ){
    frameIRQ = true
    fireIRQHandler
  }
}
