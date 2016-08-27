package com.owlandrews.nescala.helpers

import org.lwjgl.input.{Controller => LWJGLController}

trait GamePad {
  def isAButtonPressed:Boolean
  def isBButtonPressed:Boolean
  def isStartButtonPressed:Boolean
  def isSelectButtonPressed:Boolean
  def isUpButtonPressed:Boolean
  def isDownButtonPressed:Boolean
  def isLeftButtonPressed:Boolean
  def isRightButtonPressed:Boolean
  def isATurboButtonPressed:Boolean
  def isBTurboButtonPressed:Boolean
}

class DefaultGamePad(controller:LWJGLController) extends GamePad {
  private val buttons: Map[Int, Int] = Map(
    0 -> 1,
    1 -> 2,
    2 -> 9,
    3 -> 8,
    4 -> 4,
    5 -> 5,
    6 -> 6,
    7 -> 7,
    8 -> 0,
    9 -> 3
  )
  override def isAButtonPressed: Boolean = controller.isButtonPressed(buttons(0))
  override def isBButtonPressed: Boolean = controller.isButtonPressed(buttons(1))
  override def isStartButtonPressed: Boolean = controller.isButtonPressed(buttons(2))
  override def isSelectButtonPressed: Boolean =  controller.isButtonPressed(buttons(3))
  override def isUpButtonPressed: Boolean =  controller.isButtonPressed(buttons(4))
  override def isDownButtonPressed: Boolean =  controller.isButtonPressed(buttons(5))
  override def isLeftButtonPressed: Boolean =  controller.isButtonPressed(buttons(6))
  override def isRightButtonPressed: Boolean =  controller.isButtonPressed(buttons(7))
  override def isATurboButtonPressed: Boolean =  controller.isButtonPressed(buttons(8))
  override def isBTurboButtonPressed: Boolean =  controller.isButtonPressed(buttons(9))
}

class XBoxGamePad(controller:LWJGLController) extends GamePad {
  private val buttons: Map[Int, Int] = Map(
    0 -> 11,
    1 -> 12,
    2 -> 4,
    3 -> 5,
    4 -> 0,
    5 -> 1,
    6 -> 2,
    7 -> 3,
    8 -> 13,
    9 -> 14
  )
  override def isAButtonPressed: Boolean = controller.isButtonPressed(buttons(0))
  override def isBButtonPressed: Boolean = controller.isButtonPressed(buttons(1))
  override def isStartButtonPressed: Boolean = controller.isButtonPressed(buttons(2))
  override def isSelectButtonPressed: Boolean =  controller.isButtonPressed(buttons(3))
  override def isUpButtonPressed: Boolean =  controller.isButtonPressed(buttons(4))
  override def isDownButtonPressed: Boolean =  controller.isButtonPressed(buttons(5))
  override def isLeftButtonPressed: Boolean =  controller.isButtonPressed(buttons(6))
  override def isRightButtonPressed: Boolean =  controller.isButtonPressed(buttons(7))
  override def isATurboButtonPressed: Boolean =  controller.isButtonPressed(buttons(8))
  override def isBTurboButtonPressed: Boolean =  controller.isButtonPressed(buttons(9))
}

class PS4GamePad(controller:LWJGLController) extends GamePad {
  private val buttons: Map[Int, Int] = Map(
    0 -> 1,
    1 -> 2,
    2 -> 9,
    3 -> 8,
    4 -> 4,
    5 -> 5,
    6 -> 6,
    7 -> 7,
    8 -> 0,
    9 -> 3
  )
  override def isAButtonPressed: Boolean = controller.isButtonPressed(buttons(0))
  override def isBButtonPressed: Boolean = controller.isButtonPressed(buttons(1))
  override def isStartButtonPressed: Boolean = controller.isButtonPressed(buttons(2))
  override def isSelectButtonPressed: Boolean =  controller.isButtonPressed(buttons(3))
  override def isUpButtonPressed: Boolean =  controller.getAxisValue(1) < -0.5
  override def isDownButtonPressed: Boolean =  controller.getAxisValue(1) > 0.5
  override def isLeftButtonPressed: Boolean =  controller.getAxisValue(0) < -0.5
  override def isRightButtonPressed: Boolean =  controller.getAxisValue(0) > 0.5
  override def isATurboButtonPressed: Boolean =  controller.isButtonPressed(buttons(8))
  override def isBTurboButtonPressed: Boolean =  controller.isButtonPressed(buttons(9))
}

object GamePad {
  def apply(controller:LWJGLController):GamePad = controller.getName match {
      case "Wireless 360 Controller" => new XBoxGamePad(controller)
      case "Wireless Controller"     => new PS4GamePad(controller)
      case _ => new DefaultGamePad(controller)
  }
}