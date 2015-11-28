package ui

import java.awt.Canvas

import nescala.BuildInfo
import org.lwjgl.opengl.Display
import org.macrogl.Macrogl

import scala.swing.Dialog
import scala.util.{Failure, Success, Try}

case class Director(window : Canvas, audio: Audio, var view: Option[View] = None,  var menuView : Option[View] = None)(implicit gl:Macrogl) {

  var timestamp = 0L

  def SetView(view : Option[View]) {
    this.view.foreach(_.Close())
    this.view = view
    this.view.foreach(_.Open())
    timestamp = System.nanoTime()
  }

  def Step() {
    gl.clear(Macrogl.COLOR_BUFFER_BIT)
    // Clear the screen and depth buffer
    val dt = System.nanoTime() - timestamp
    timestamp = System.nanoTime()
    val seconds = dt / 1000000000.0F
    view.foreach(_.Update(seconds))
  }

  def Start(path: String) = loadView(path) match {
            case Success(_) => run()
            case Failure(e) => Dialog.showMessage(new {def peer = window.getParent}, e.getMessage, BuildInfo.name, Dialog.Message.Warning)
  }

  private def loadView(path: String) = Try(nescala.Console(path, audio)).map(console => SetView(Some(GameView(console, audio, window))))

  private def run() = while (!Display.isCloseRequested) {
      Step()
      Display.update(true)
  }

  def Close = SetView(None)
}
