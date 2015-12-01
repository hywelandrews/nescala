package ui

import java.nio.ByteBuffer
import javax.sound.sampled._

import scala.util.Try

class Audio {

  val sampleRate = 44100

  val format = new AudioFormat(
    sampleRate,
    16,   //bit
    2,    //channel
    true, //signed
    false  //little endian
  )

  val samplesPerFrame = Math.ceil((sampleRate * 2) / 60F).toInt
  val buffer = ByteBuffer.allocate(4)
  val output = Try {
    val sdl = AudioSystem.getSourceDataLine(format)
    sdl.open(format, samplesPerFrame * 4 /*frame*/ * 2 /*ch*/ * 2 /*bytes/sample*/)
    sdl.start()
    sdl
  }

  def stop() = output.foreach(x => x.close())

  def receive(sample:Float) = output.withFilter(_.available() >= 4).foreach{ sdl =>
    val channel = buffer.putFloat(sample * 0.8F).array()
    buffer.rewind()
    sdl.write(channel, 0, 4)
  }

}
