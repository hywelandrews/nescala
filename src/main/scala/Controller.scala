package nescala

object Controller {
  val ButtonA = 0
  val ButtonB = 1
  val ButtonSelect = 2
  val ButtonStart = 3
  val ButtonUp = 4
  val ButtonDown = 5
  val ButtonLeft = 6
  val ButtonRight = 7
}

case class Controller() {
  var Buttons:Map[Int, Boolean] = Map.empty[Int, Boolean]
  var Index, Strobe:Int = 0

  def Read():Int = {
    var value:Int = 0
    if (Index < 8 && Buttons(Index)) value = 1

    Index = Index + 1

    if ((Strobe & 1) == 1) Index = 0
    value
  }

  def Write(value:Int) = {
    Strobe = value
    if ((Strobe & 1) == 1) Index = 0
  }

  def SetButtons(buttons:Map[Int, Boolean]) = Buttons = buttons
}
