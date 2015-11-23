package ui

import java.nio.ByteBuffer
import javax.sound.sampled._

import scala.concurrent.Future
import scala.util.Try

class Audio {

  val sampleRate = 44100
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  val format = new AudioFormat(
    sampleRate,
    16,   //bit
    1,    //channel
    true, //signed
    true  //little endian
  )

  val samplesPerFrame = Math.ceil(sampleRate / 60).toInt * 4
  val buffer = ByteBuffer.allocate(4)
  val output = Try {
    val sdl = AudioSystem.getSourceDataLine(format)
    sdl.open(format, samplesPerFrame)
    sdl.start()
    sdl
  }

  def stop() = output.foreach(x => x.close())

  def receive(sample:Float) = Future {
    val channel = buffer.putFloat(sample).array().take(2)
    output.foreach{sdl =>
      if(sdl.available() > 2) sdl.write(channel, 0, 2)
    }
    buffer.rewind()
  }
}
