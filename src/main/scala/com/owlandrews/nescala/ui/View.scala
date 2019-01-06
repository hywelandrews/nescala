package com.owlandrews.nescala.ui

import java.awt._
import javax.swing.ImageIcon
import javax.swing.border.EmptyBorder

import org.lwjgl.input.Keyboard
import org.lwjgl.input.{Controllers => LWJGLControllers}
import org.lwjgl.opengl.GL11

import scala.language.postfixOps
import scala.swing.event._
import scala.swing.{Button, Action, Alignment}
import com.owlandrews.nescala.helpers._
import com.owlandrews.nescala.{Cartridge, Console, Controller}


trait View {
  def Open()
  def Close()
  def Reset()
  def Update(dt:Double)
  def Save()
}

case class GameView(console:Console, window: Canvas) extends View {
  val width = 256
  val padding = 0

  private val texture = Texture.createTexture()

  val defaultInput = Map(
    Controller.ButtonA      -> false,
    Controller.ButtonB      -> false,
    Controller.ButtonStart  -> false,
    Controller.ButtonSelect -> false,
    Controller.ButtonUp     -> false,
    Controller.ButtonDown   -> false,
    Controller.ButtonLeft   -> false,
    Controller.ButtonRight  -> false
  )

  lazy val getGamePad:Option[GamePad] =
    Option.apply[GamePad](if(LWJGLControllers.getControllerCount > 0) GamePad(LWJGLControllers.getController(0)) else null)

  override def Open(): Unit = {
    LWJGLControllers.create()
    LWJGLControllers.poll()
    Keyboard.create()
    Keyboard.poll()

    Audio.start()

    GL11.glClearColor(0, 0, 0, 1)
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  override def Close(): Unit = {
    LWJGLControllers.destroy()
    Keyboard.destroy()
    Audio.stop()
  }

  override def Update(dt:Double): Unit = {
    val seconds = if (dt > 1) 0 else dt
    updateControllers()
    console.StepSeconds(seconds)
    GL11.glBindTexture(GL11.GL_TEXTURE_2D, texture)
    Texture.setTexture(console.VideoBuffer())
    drawBuffer()
    GL11.glBindTexture(GL11.GL_TEXTURE_2D, 0)
  }

  override def Reset(): Unit = console.Reset()

  override def Save(): Unit = File.SaveState(console)

  private def drawBuffer() {
    val frameBounds = window.getBounds
    val h = frameBounds.height
    val w = frameBounds.width
    val s1 = w / 256
    val s2 = h / 240
    val f = 1 - padding
    val (x, y) = if (s1 >= s2) (f * s2 / s1, f)
                 else (f, f * s1 / s2)

    GL11.glViewport(0, 0, w, h)

    GL11.glBegin(GL11.GL_QUADS)
    GL11.glTexCoord2f(0, 1)
    GL11.glVertex2f(-x, -y)
    GL11.glTexCoord2f(1, 1)
    GL11.glVertex2f(x, -y)
    GL11.glTexCoord2f(1, 0)
    GL11.glVertex2f(x, y)
    GL11.glTexCoord2f(0, 0)
    GL11.glVertex2f(-x, y)
    GL11.glEnd()
  }

  private def readKeys(turbo:Boolean):Map[Int, Boolean] = Map(
    Controller.ButtonA      -> (Keyboard.isKeyDown(Keyboard.KEY_Z) || (turbo && Keyboard.isKeyDown(Keyboard.KEY_A))),
    Controller.ButtonB      -> (Keyboard.isKeyDown(Keyboard.KEY_X) || (turbo && Keyboard.isKeyDown(Keyboard.KEY_S))),
    Controller.ButtonStart  -> Keyboard.isKeyDown(Keyboard.KEY_RETURN),
    Controller.ButtonSelect -> Keyboard.isKeyDown(Keyboard.KEY_RSHIFT),
    Controller.ButtonUp     -> Keyboard.isKeyDown(Keyboard.KEY_UP),
    Controller.ButtonDown   -> Keyboard.isKeyDown(Keyboard.KEY_DOWN),
    Controller.ButtonLeft   -> Keyboard.isKeyDown(Keyboard.KEY_LEFT),
    Controller.ButtonRight  -> Keyboard.isKeyDown(Keyboard.KEY_RIGHT)
  )

  private def readGamePad(gamePad:GamePad, turbo:Boolean) = Map(
      Controller.ButtonA      -> (gamePad.isAButtonPressed || (turbo && gamePad.isATurboButtonPressed)),
      Controller.ButtonB      -> (gamePad.isBButtonPressed || (turbo && gamePad.isBTurboButtonPressed)),
      Controller.ButtonStart  -> gamePad.isStartButtonPressed,
      Controller.ButtonSelect -> gamePad.isSelectButtonPressed,
      Controller.ButtonUp     -> gamePad.isUpButtonPressed,
      Controller.ButtonDown   -> gamePad.isDownButtonPressed,
      Controller.ButtonLeft   -> gamePad.isLeftButtonPressed,
      Controller.ButtonRight  -> gamePad.isRightButtonPressed
  )

  private def updateControllers() = {
    val turbo = (console.ppu.Frame % 6) < 3
    console.controller1.SetButtons(getGamePad.map(gamePad => readGamePad(gamePad, turbo)).getOrElse(readKeys(turbo)))
    console.controller2.SetButtons(getGamePad.map(_ => readKeys(turbo)).getOrElse(defaultInput))
  }
}

case class MenuView(window: WrapPanel) extends View {

  override def Open(): Unit = if(!window.visible) {
      window.visible = true
      window.contents ++= (Settings.gameLibrary match {
        case Some(gameLibraryPath) => openGameLibrary(gameLibraryPath)
        case None => Iterable(selectGameLibrary("Select game library"))
      })
  }

  override def Update(dt: Double): Unit = ()

  override def Close(): Unit = {
    window.peer.revalidate()
  }

  override def Reset(): Unit = {
    window.peer.removeAll()
    window.visible = false
    Close()
    Open()
  }

  private def selectGameLibrary(message:String) = new Button {
    border = new EmptyBorder(10, 0, 0, 0)
    opaque = false
    foreground = new Color(192,197,206)
    cursor = new Cursor(Cursor.HAND_CURSOR)
    action = Action(s"<html>$message</html>")(Run.OpenFolderDialog())
    listenTo(mouse.moves)
    reactions += {
      case MouseEntered(s, p, m) => foreground = Color.WHITE
      case MouseExited(s, p, m) => foreground = new Color(192,197,206)
    }
  }

  private def gameIcon(name:String, image:ImageIcon, filePath:String) = new Button(name){
    opaque = false
    contentAreaFilled = false
    border = new EmptyBorder(0, 0, 0, 0)
    action = Action(name)(Run.StartDisplay(filePath))
    icon = image
    iconTextGap = 5
    font = new Font("Lucida Grande", Font.BOLD, 10)
    foreground = Color.WHITE
    preferredSize = new swing.Dimension(Thumbnail.x  + Thumbnail.padding, Thumbnail.y + Thumbnail.padding)
    verticalTextPosition = Alignment.Bottom
    horizontalTextPosition = Alignment.Center
  }

  private def openGameLibrary(path:String): Iterable[Button] = {
    val romFolder = new java.io.File(path)
    val romFiles = romFolder.listFiles(File.Filter).toList
    val thumbnail = new Thumbnail(window.background)

    val roms = romFiles.par.map { file =>
      val cart = Cartridge(file.getAbsolutePath)
      Rom(cart.CRC, file.getName, file.getAbsolutePath, thumbnail)
    }.toList

    if (roms.isEmpty) Iterable(selectGameLibrary("No games found. Select game library"))
    else roms.map { rom =>
      val icon = gameIcon(rom.title, new ImageIcon(rom.thumbnail), rom.path)
      icon.peer.setText(rom.title) // Needed because adding Actions to Buttons overwrites their text
      icon
    }
  }

  override def Save(): Unit = ()
}
