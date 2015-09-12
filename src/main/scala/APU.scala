/**
 * Created by Hywel on 5/1/15.
 */
class APU {
  private val frameCounterRate = CPU.frequency / 240.0
  private val sampleRate = CPU.frequency / 44100.0 / 2
  private case class Length(var enabled:Boolean = false, var value:Int = 0,
                            table:Array[Int] = Array(10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
                                          12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30))
  private case class Timer(var period:Int = 0, var value:Int = 0)
  private case class Duty (var mode:Int = 0, var value:Int = 0)
  private case class Envelope (var enabled:Boolean = false, var loop :Boolean = false, var start: Boolean = false,
                               var period:Int = 0, var value:Byte = 0, var volume:Byte = 0)
  private class Pulse {
    case class Sweep(var reload:Boolean = false, var enabled:Boolean = false, var negate:Boolean = false,
                     var shift: Int = 0, var period:Int = 0,var value:Int = 0)
    var enabled:Boolean = false
    var channel:Int = 0
    var length:Length = Length()
    val timer:Timer = Timer()
    val duty:Duty = Duty()
    val sweep:Sweep = Sweep()
    val envelope:Envelope = Envelope()
    var constantVolume:Int = 0

    def writeControl(value:Int) = {
      duty.mode = (value >> 6) & 3
      length.enabled = ((value >> 5) & 1) == 0
      envelope.loop = ((value >> 5) & 1) == 1
      envelope.enabled = ((value >> 4) & 1) == 0
      envelope.period = value & 15
      constantVolume = value & 15
      envelope.start = true
    }

    def writeSweep(value:Int) = {
      sweep.enabled = ((value >> 7) & 1) == 1
      sweep.period = (value >> 4) & 7
      sweep.negate = ((value >> 3) & 1) == 1
      sweep.shift = value & 7
      sweep.reload = true
    }

    def writeTimerLow(value:Int) = timer.period = (timer.period & 0xFF00) | value

    def writeTimerHigh(value:Int) = {
      length.value = length.table(value >> 3)
      timer.period = (timer.period & 0x00FF) | ((value & 7) << 8)
      envelope.start = true
      duty.value = 0
    }
  }

  private class Triangle {
      var enabled:Boolean = false
      val length:Length = Length()
      val timer:Timer = Timer()
      val duty:Duty = Duty()
      var counterPeriod:Int = 0
      var counterValue:Int = 0
      var counterReload:Boolean = false

     def writeControl(value:Int) = {
      length.enabled = ((value >> 7) & 1) == 0
      counterPeriod = value & 0x7F
    }
    def writeTimerLow(value:Int) = timer.period = (timer.period & 0xFF00) | value

    def writeTimerHigh(value:Int) = {
      length.value = length.table(value >> 3)
      timer.period = (timer.period & 0x00FF) | (value & 7  << 8)
      timer.value = timer.period
      counterReload = true
    }
  }

  private class Noise {
    var enabled:Boolean = false
    var mode:Boolean = false
    var shiftRegister:Int = 0
    val length:Length = Length()
    val timer:Timer = Timer()
    val envelope:Envelope = Envelope()
    var constantVolume:Int = 0
    private val table = Array( 4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068)
    def writeControl(value:Int) {
      length.enabled = ((value >> 5) & 1) == 0
      envelope.loop = ((value >> 5) & 1) == 1
      envelope.enabled = ((value >> 4) & 1) == 0
      envelope.period = value & 15
      constantVolume = value & 15
      envelope.start = true
    }

    def writePeriod(value:Int) {
      mode = (value & 0x80) == 0x80
      timer.period = table(value & 0x0F)
    }

    def writeLength(value:Int) {
      length.value = length.table(value >> 3)
      envelope.start = true
    }
  }

  private def writeControl(value:Int) {
    pulse1.enabled = (value & 1) == 1
    pulse2.enabled = (value & 2) == 2
    triangle.enabled = (value & 4) == 4
    noise.enabled = (value & 8) == 8
    if (!pulse1.enabled) pulse1.length.value = 0
    if (!pulse2.enabled) pulse2.length.value = 0
    if (!triangle.enabled) triangle.length.value = 0
    if (!noise.enabled) noise.length.value = 0
  }

  private def writeFrameCounter(value:Int) = {
    framePeriod = 4 + (value >> 7) & 1
    frameIRQ = ((value >> 6) & 1) == 0
    // apu.frameValue = 0
    // if apu.framePeriod == 5 {
    // 	apu.stepEnvelope()
    // 	apu.stepSweep()
    // 	apu.stepLength()
    // }
  }

  private val pulse1 = new Pulse
  private val pulse2 = new Pulse
  private val triangle = new Triangle
  private val noise = new Noise
  private var framePeriod:Int = 0
  private var frameValue:Int = 0
  private var frameIRQ:Boolean = false

  def ReadRegister(address: Int): Int = ???

  def WriteRegister(address: Int, value: Int) = address match {
    case pulse1Control   if address == 0x4000 => pulse1.writeControl(value)
    case pulse1Sweep     if address == 0x4001 => pulse1.writeSweep (value)
    case pulse1Timer     if address == 0x4002 => pulse1.writeTimerLow (value)
    case pulse1Timer     if address == 0x4003 => pulse1.writeTimerHigh (value)
    case pulse2Control   if address == 0x4004 => pulse2.writeControl (value)
    case pulse2Sweep     if address == 0x4005 => pulse2.writeSweep (value)
    case pulse2Timer     if address == 0x4006 => pulse2.writeTimerLow (value)
    case pulse2Timer     if address == 0x4007 => pulse2.writeTimerHigh (value)
    case triangleControl if address == 0x4008 => triangle.writeControl (value)
    case triangleTimer   if address == 0x4009 =>
    case triangleTimer   if address == 0x400A => triangle.writeTimerLow (value)
    case triangleTimer   if address == 0x400B => triangle.writeTimerHigh (value)
    case noiseControl    if address == 0x400C => noise.writeControl(value)
    case noisePeriod     if address == 0x400D =>
    case noisePeriod     if address == 0x400E => noise.writePeriod(value)
    case noiseLength     if address == 0x400F => noise.writeLength (value)
    case apu             if address == 0x4015 => writeControl(value)
    case apu             if address == 0x4017 => writeFrameCounter (value)
  }

  def Step (): Int = 0
}
