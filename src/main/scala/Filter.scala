package nescala

trait Filter {
  def Step(x:Float):Float
}
// First order filters are defined by the following parameters.
// y[n] = B0*x[n] + B1*x[n-1] - A1*y[n-1]
case class FirstOrderFilter(B0:Float, B1:Float, A1:Float) extends Filter {
  private var prevX = 0F
  private var prevY = 0F

  def Step(x:Float):Float = {
    val y = (B0 * x) + (B1 * prevX) - (A1 * prevY)
    prevY = y
    prevX = x
    y
  }
}

case class FilterChain(filters:List[Filter]){
  def Step(x:Float):Float = filters.foldLeft(x)((y, filter) => filter.Step(y))
}

object FilterChain {
  def apply(sampleRate:Float):FilterChain = sampleRate match {
    case 0 => FilterChain(List.empty[Filter])
    case _ => FilterChain(initializeFilters(sampleRate))
  }

  private def initializeFilters(sampleRate:Float) =
      Filter.HighPassFilter(sampleRate, 90) ::
      Filter.HighPassFilter(sampleRate, 440) ::
      Filter.LowPassFilter(sampleRate, 14000) ::
      Nil
}

object Filter {
  // sampleRate: samples per second
  // cutoffFreq: oscillations per second
  def LowPassFilter(sampleRate: Float, cutoffFreq: Float): Filter = {
    val c = sampleRate / math.Pi.toFloat / cutoffFreq
    val a0i = 1 / (1 + c)
    FirstOrderFilter(
      B0 = a0i,
      B1 = a0i,
      A1 = (1 - c) * a0i
    )
  }

  def HighPassFilter(sampleRate: Float, cutoffFreq: Float): Filter = {
    val c = sampleRate / math.Pi.toFloat / cutoffFreq
    val a0i = 1 / (1 + c)
    FirstOrderFilter(
      B0 = c * a0i,
      B1 = -c * a0i,
      A1 = (1 - c) * a0i
    )
  }
}
