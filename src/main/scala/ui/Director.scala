package ui

import java.awt.Canvas

import nescala.BuildInfo
import org.lwjgl.opengl.Display
import org.macrogl.Macrogl

import scala.swing.Dialog
import scala.util.{Failure, Success, Try}

case class Director(window : Canvas, audio: Audio)(implicit gl:Macrogl) {

  var timestamp = 0L
  var view: Option[View] = None

  def Step() {
    gl.clear(Macrogl.COLOR_BUFFER_BIT)
    // Clear the screen and depth buffer
    val dt = System.nanoTime() - timestamp
    timestamp = System.nanoTime()
    val seconds = dt / 1000000000.0F
    view.foreach(_.Update(seconds))
  }

  def Reset = view.foreach(x => x.Reset())

  def Close = setView(None)

  def Start(path: String) = loadView(path) match {
            case Success(_) => run()
            case Failure(e) => Dialog.showMessage(new {def peer = window.getParent}, e.getMessage, BuildInfo.name, Dialog.Message.Warning)
  }

  private def loadView(path: String) = Try(nescala.Console(path, audio)).map(console => setView(Some(GameView(console, audio, window))))

  private def setView(view : Option[View]) {
    this.view.foreach(_.Close())
    this.view = view
    this.view.foreach(_.Open())
    timestamp = System.nanoTime()
  }

  private def run() = {
    while (view.isDefined) {
      Step()
      Display.sync(60)
      Display.update(true)
    }
    Display.destroy()
  }
}
