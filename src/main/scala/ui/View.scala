package ui

import java.awt.Canvas

import nescala.{Console, Controller}
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.{Display, GL11}
import org.macrogl.Macrogl

object Start

trait View {
  def Open()(implicit gl: Macrogl)
  def Close()
  def Update(dt:Double)(implicit gl: Macrogl)
}

case class GameView(console:Console, audio:Audio, window: Canvas) extends View {
  val width = 256
  val padding = 0

  private val texture = Texture.createTexture()

  val defaultJoystick = Map(
    Controller.ButtonA      -> false,
    Controller.ButtonB      -> false,
    Controller.ButtonStart  -> false,
    Controller.ButtonSelect -> false,
    Controller.ButtonUp     -> false,
    Controller.ButtonDown   -> false,
    Controller.ButtonLeft   -> false,
    Controller.ButtonRight  -> false
  )

  def drawBuffer()(implicit gl: Macrogl) {
    val frameBounds = window.getBounds
    val h = frameBounds.height
    val w = frameBounds.width
    val s1 = w / 256
    val s2 = h / 240
    val f = 1 - padding
    val (x, y) = if (s1 >= s2) (f * s2 / s1, f)
                 else (f, f * s1 / s2)

    GL11.glViewport(0, 0, w, h)

    GL11.glBegin(GL11.GL_QUADS);
    {
      GL11.glTexCoord2f(0, 1)
      GL11.glVertex2f(-x, -y)
      GL11.glTexCoord2f(1, 1)
      GL11.glVertex2f(x, -y)
      GL11.glTexCoord2f(1, 0)
      GL11.glVertex2f(x, y)
      GL11.glTexCoord2f(0, 0)
      GL11.glVertex2f(-x, y)
    }
    GL11.glEnd()
  }

  override def Open()(implicit gl: Macrogl): Unit = {
    gl.clearColor(0, 0, 0, 1)
    gl.enable(Macrogl.TEXTURE_2D)
  }

  override def Close(): Unit = {
    Display.destroy()
    audio.stop()
  }

  override def Update(dt:Double)(implicit gl: Macrogl): Unit = {
    val seconds = if (dt > 1) 0 else dt
    updateControllers
    console.StepSeconds(seconds)
    gl.bindTexture(Macrogl.TEXTURE_2D, texture)
    Texture.setTexture(console.VideoBuffer())
    drawBuffer()
    gl.bindTexture(Macrogl.TEXTURE_2D, 0)
  }

  private def readKeys(turbo:Boolean):Map[Int, Boolean] = Map(
    Controller.ButtonA      -> Keyboard.isKeyDown(Keyboard.KEY_Z),
    Controller.ButtonB      -> Keyboard.isKeyDown(Keyboard.KEY_X),
    Controller.ButtonStart  -> Keyboard.isKeyDown(Keyboard.KEY_RETURN),
    Controller.ButtonSelect -> Keyboard.isKeyDown(Keyboard.KEY_RSHIFT),
    Controller.ButtonUp     -> Keyboard.isKeyDown(Keyboard.KEY_UP),
    Controller.ButtonDown   -> Keyboard.isKeyDown(Keyboard.KEY_DOWN),
    Controller.ButtonLeft   -> Keyboard.isKeyDown(Keyboard.KEY_LEFT),
    Controller.ButtonRight  -> Keyboard.isKeyDown(Keyboard.KEY_RIGHT)
  )

  def readJoystick(turbo:Boolean) = {

    defaultJoystick
//    val axes = GetJoystickAxes(joy)
//    val buttons = GetJoystickButtons(joy)
//    result[ButtonA] = buttons[0] == 1 || (turbo && buttons[2] == 1)
//    result[ButtonB] = buttons[1] == 1 || (turbo && buttons[3] == 1)
//    result[ButtonSelect] = buttons[6] == 1
//    result[ButtonStart] = buttons[7] == 1
//    result[ButtonUp] = axes[1] < -0.5
//    result[ButtonDown] = axes[1] > 0.5
//    result[ButtonLeft] = axes[0] < -0.5
//    result[ButtonRight] = axes[0] > 0.5
//    return result
  }

  private def updateControllers = {
    val turbo = (console.ppu.frame % 6) < 3
//    val j1 = readJoystick(Joystick1, turbo)
//    val j2 = readJoystick(Joystick2, turbo)
    console.controller1.SetButtons(readKeys(turbo))
    console.controller2.SetButtons(readJoystick(turbo))
  }
}
