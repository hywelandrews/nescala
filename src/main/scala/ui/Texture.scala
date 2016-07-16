package ui

import java.awt.image._
import java.nio.IntBuffer

import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{GL11, GL12}

object Texture {
  private val buffer = BufferUtils.createIntBuffer(256 * 240 * 4)

  def createTexture():Int = {
    val texture = GL11.glGenTextures()
    GL11.glBindTexture(GL11.GL_TEXTURE_2D, texture)
    GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_MIN_FILTER, GL11.GL_NEAREST)
    GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_MAG_FILTER, GL11.GL_NEAREST)
    GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_S, GL12.GL_CLAMP_TO_EDGE)
    GL11.glTexParameteri(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_T, GL12.GL_CLAMP_TO_EDGE)
    GL11.glBindTexture(GL11.GL_TEXTURE_2D, 0)
    texture
  }

  def setTexture(im:BufferedImage) = {

    val dataBuffer = IntBuffer.wrap(im.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData)

    buffer.put(dataBuffer)
    buffer.flip()//FOR THE LOVE OF GOD DO NOT FORGET THIS
    // Use GL_UNSIGNED_INT_8_8_8_8_REV to reverse RGBA pixel format to BGRA on GPU
    GL11.glTexImage2D(GL11.GL_TEXTURE_2D, 0, GL11.GL_RGBA, im.getWidth, im.getHeight, 0, GL12.GL_BGRA, GL12.GL_UNSIGNED_INT_8_8_8_8_REV, buffer)
    buffer.rewind()
  }
}