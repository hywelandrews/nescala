package ui

import java.awt._
import javax.swing.ImageIcon
import javax.swing.border.EmptyBorder

import helpers.{File, Rom, Settings, Thumbnail}
import nescala.{Cartridge, Console, Controller}
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.GL11

import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.swing.event.{MouseEntered, MouseExited}
import scala.swing.{Button, _}

trait View {
  def Open()
  def Close()
  def Reset()
  def Update(dt:Double)
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

  override def Open(): Unit = {
    audio.start()
    GL11.glClearColor(0, 0, 0, 1)
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  override def Close(): Unit = {
    audio.stop()
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

  override def Reset(): Unit = console.Reset

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
    Controller.ButtonA      -> Keyboard.isKeyDown(Keyboard.KEY_Z),
    Controller.ButtonB      -> Keyboard.isKeyDown(Keyboard.KEY_X),
    Controller.ButtonStart  -> Keyboard.isKeyDown(Keyboard.KEY_RETURN),
    Controller.ButtonSelect -> Keyboard.isKeyDown(Keyboard.KEY_RSHIFT),
    Controller.ButtonUp     -> Keyboard.isKeyDown(Keyboard.KEY_UP),
    Controller.ButtonDown   -> Keyboard.isKeyDown(Keyboard.KEY_DOWN),
    Controller.ButtonLeft   -> Keyboard.isKeyDown(Keyboard.KEY_LEFT),
    Controller.ButtonRight  -> Keyboard.isKeyDown(Keyboard.KEY_RIGHT)
  )

  private def readJoystick(turbo:Boolean) = {

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

  private def updateControllers() = {
    val turbo = (console.ppu.Frame % 6) < 3
//    val j1 = readJoystick(Joystick1, turbo)
//    val j2 = readJoystick(Joystick2, turbo)
    console.controller1.SetButtons(readKeys(turbo))
    console.controller2.SetButtons(readJoystick(turbo))
  }
}

case class MenuView(window: WrapPanel) extends View {

  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

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
    Close()
    Open()
  }

  private def selectGameLibrary(message:String) = new Button {
    border = new EmptyBorder(10, 0, 0, 0)
    opaque = false
    foreground = new Color(192,197,206)
    cursor = new Cursor(Cursor.HAND_CURSOR)
    action = Action(s"<html>$message</html>")(Run.OpenFolderDialog)
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

    import scala.concurrent.duration._

    val romResult = Await.result(Future.sequence(romFiles map { file =>
        Future(Cartridge(file.getAbsolutePath)) map { cartridge =>
          cartridge.CRC -> Rom(cartridge.CRC, file.getName, file.getAbsolutePath, thumbnail)
        }
    }), 2 minutes)

    if (romResult.isEmpty) Iterable(selectGameLibrary("No games found. Select game library"))
    else romResult.map { case (crc, rom) =>
      val icon = gameIcon(rom.title, new ImageIcon(rom.thumbnail), rom.path)
      icon.peer.setText(rom.title) // Needed because adding Actions to Buttons overwrites their text
      icon
    }
  }
}
