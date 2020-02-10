package com.owlandrews.nescala.ui

import javax.sound.sampled._

import scala.util.Try

object Audio {

  private val sampleRate = 44100f

  private val format = new AudioFormat(
    sampleRate,
    16,   //bit
    2,    //channel
    true, //signed
    false  //little endian
  )

  private val samplesPerFrame = Math.ceil((sampleRate * 2) / 60F).toInt
  private val buffer = new scala.collection.mutable.ArrayBuffer[Byte](samplesPerFrame)

  private val output = Try {
    val sdl = AudioSystem.getSourceDataLine(format)
    sdl.open(format, samplesPerFrame * 4 /*frame*/ * 2 /*ch*/ * 2 /*bytes/sample*/)
    sdl
  }

  def start() = output.foreach(x => x.start())

  def stop() = output.foreach(x => x.stop())

  def receive(sample:Int): Unit = {
    val outputVolume = sample * 0.799987793F

    val outputSample = if (outputVolume < -32768) -32768
                         else if (outputVolume > 32767) 32767
                         else outputVolume

    //left ch//left ch

    0
    val lch = outputSample.toInt
    val o1 = (lch & 0xff).toByte
    val o2  = ((lch >> 8) & 0xff).toByte
    //right ch
    val rch = outputSample.toInt
    val o3  = (rch & 0xff).toByte
    val o4 = ((rch >> 8) & 0xff).toByte

    buffer ++= Array(o1, o2, o3, o4)

    output.foreach{sdl =>
     if(sdl.available() >= buffer.length) {
       sdl.write(buffer.toArray, 0, buffer.size)
     }
      buffer.clear()
    }
  }
}
