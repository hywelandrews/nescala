package ui

import java.nio.{ByteBuffer, ByteOrder}
import javax.sound.sampled._

import scala.util.Try

class Audio {

  private val sampleRate = 44100f

  private val format = new AudioFormat(
    sampleRate,
    16,   //bit
    2,    //channel
    true, //signed
    false  //little endian
  )

  private val samplesPerFrame = Math.ceil((sampleRate * 2) / 60F).toInt
  private val buffer = new scala.collection.mutable.ArrayBuffer[Byte]()

  private val output = Try {
    val sdl = AudioSystem.getSourceDataLine(format)
    sdl.open(format, samplesPerFrame * 4 /*frame*/ * 2 /*ch*/ * 2 /*bytes/sample*/)
    sdl
  }

  def start() = output.foreach(x => x.start())

  def stop() = output.foreach(x => x.stop())

  def receive(sample:Int) = {
    val outputVolume = (sample * 0.799987793).toInt

    val outputSample = if (outputVolume < -32768) -32768
                       else if (outputVolume > 32767) 32767
                       else outputVolume

    val channel = ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN).putInt(outputSample).array()

    buffer ++= Array(channel(0),channel(1),channel(0), channel(1))

    output.filter(_.available() >= buffer.length).foreach{ sdl =>
      sdl.write(buffer.toArray, 0, buffer.length)
      buffer.clear()
    }
  }
}
