package com.owlandrews.nescala

object Controller {
  val ButtonA       = 0
  val ButtonB       = 1
  val ButtonSelect  = 2
  val ButtonStart   = 3
  val ButtonUp      = 4
  val ButtonDown    = 5
  val ButtonLeft    = 6
  val ButtonRight   = 7
}

case class Controller() {
  private val buttons = scala.collection.concurrent.TrieMap(
    Controller.ButtonA -> false,
    Controller.ButtonB -> false,
    Controller.ButtonStart -> false,
    Controller.ButtonSelect -> false,
    Controller.ButtonUp -> false,
    Controller.ButtonDown -> false,
    Controller.ButtonLeft -> false,
    Controller.ButtonRight -> false
  )

  private var index, strobe = 0

  def Read():Int = {
    val value = if (index < 8 && buttons(index)) 1 else 0

    index = index + 1

    if ((strobe & 1) == 1) index = 0
    value
  }

  def Write(value:Int) = {
    strobe = value
    if ((strobe & 1) == 1) index = 0
  }

  def SetButtons(buttons:Map[Int, Boolean]) = buttons.foreach(b => this.buttons.update(b._1, b._2))
}
