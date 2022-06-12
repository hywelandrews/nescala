package com.owlandrews.nescala.ui

import java.awt.{Canvas, Container}
import com.owlandrews.nescala.BuildInfo
import org.lwjgl.opengl.{Display, GL11}

import scala.swing.Dialog
import scala.util.{Failure, Success, Try}

case class Director(gameWindow : Canvas, menuWindow: WrapPanel) {
  
  private var view : Option[View] = None
  private var pause = false

  def Menu(): Unit = setView(Some(MenuView(menuWindow)))

  def Reset(): Unit = view.foreach(_.Reset())

  def Close(): Unit = setView(None)

  def Pause(): Unit = pause = true

  def Resume(): Unit = pause = false

  def Save(): Unit = view.foreach(_.Save())

  def Start(path: String): Unit = loadGameView(path) match {
            case Success(_) => run()
            case Failure(e) => Dialog.showMessage(new {def peer: Container = gameWindow.getParent}, e.getMessage, BuildInfo.name, Dialog.Message.Warning)
  }

  private def loadGameView(path: String) = Try(com.owlandrews.nescala.Console(path)).map(console => setView(Some(GameView(console, gameWindow))))

  private def setView(view : Option[View]): Unit = {
    this.view.foreach(_.Close())
    this.view = view
    this.view.foreach(_.Open())
  }

  private def step(ts:Long) = {
    // Clear the screen and depth buffer
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT)

    val dt = System.nanoTime() - ts
    val nextTimeStamp = System.nanoTime()
    val seconds = dt / 1000000000.0F
    view.foreach(_.Update(seconds))
    nextTimeStamp
  }

  private def run(): Unit = {
    var timestamp = System.nanoTime()
    while (view.isDefined) {
      if (!pause) timestamp = step(timestamp)
      Display.update()
    }
    Display.destroy()
  }
}
