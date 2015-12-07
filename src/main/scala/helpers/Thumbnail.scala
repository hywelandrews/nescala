package helpers

import java.awt.Color
import java.awt.Dimension
import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.awt._

import scala.swing.Graphics2D
import scala.swing._

class Thumbnail(color: Color) {
  def Resize(image:Image) = {
    val thumbnail = image.getScaledInstance(Thumbnail.x, Thumbnail.y, Image.SCALE_SMOOTH)
    val arcs = new Dimension(10, 10)
    val output = new BufferedImage(Thumbnail.x,  Thumbnail.y, BufferedImage.TYPE_INT_ARGB)
    val outputGraphics = output.getGraphics.asInstanceOf[Graphics2D]
    outputGraphics.setComposite(AlphaComposite.Src)
    outputGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    outputGraphics.setColor(color)
    outputGraphics.fill(new RoundRectangle2D.Float(0, 0, Thumbnail.x,  Thumbnail.y, arcs.width, arcs.height))
    outputGraphics.setComposite(AlphaComposite.SrcAtop)
    outputGraphics.drawImage(thumbnail, 0, 0, null)
    outputGraphics.dispose()
    output
  }
}

object Thumbnail {
  val x = 90
  val y = 120
  val padding = 25
}
