package nescala

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
  private var buttons = Map(
    Controller.ButtonA      -> false,
    Controller.ButtonB      -> false,
    Controller.ButtonStart  -> false,
    Controller.ButtonSelect -> false,
    Controller.ButtonUp     -> false,
    Controller.ButtonDown   -> false,
    Controller.ButtonLeft   -> false,
    Controller.ButtonRight  -> false
  )

  private var index, strobe:Int = 0

  def Read():Int = {
    var value:Int = 0
    if (index < 8 && buttons(index)) value = 1

    index = index + 1

    if ((strobe & 1) == 1) index = 0
    value
  }

  def Write(value:Int) = {
    strobe = value
    if ((strobe & 1) == 1) index = 0
  }

  def SetButtons(buttons:Map[Int, Boolean]) = this.buttons = buttons
}
