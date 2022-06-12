package com.owlandrews.nescala.ui

import java.awt._
import javax.swing.{JPanel, JScrollPane, SwingUtilities}

import scala.swing.{FlowPanel, Panel, SequentialContainer}

/**
 * Taken from https://svn.kwarc.info/repos/MMT/src/mmt-api/trunk/src/main/info/kwarc/mmt/api/gui/Swing.scala
 */

class WrapPanel(defaultWidth: Int, alignment: FlowPanel.Alignment.Value)(contents0: scala.swing.Component*) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer: JPanel =
    new JPanel(new WrapLayout(defaultWidth)) with SuperMixin
  def this(defaultWidth:Int, contents0: scala.swing.Component*) = this(defaultWidth, FlowPanel.Alignment.Center)(contents0: _*)
  def this(defaultWidth:Int) = this(defaultWidth, FlowPanel.Alignment.Center)()

  contents ++= contents0

  private def layoutManager = peer.getLayout.asInstanceOf[WrapLayout]

  def vGap: Int = layoutManager.getVgap
  def vGap_=(n: Int): Unit = { layoutManager.setVgap(n) }
  def hGap: Int = layoutManager.getHgap
  def hGap_=(n: Int): Unit = { layoutManager.setHgap(n) }
}

class WrapLayout(defaultWidth: Int, align: Int = FlowLayout.CENTER, hgap: Int = 5, vgap: Int = 5)
  extends FlowLayout(align, hgap, vgap) {

  override def preferredLayoutSize(target: Container) = layoutSize(target, true)
  override def minimumLayoutSize(target: Container) = {
    val minimum = layoutSize(target, false)
    minimum.width -= getHgap + 1
    minimum
  }
  private def layoutSize(target: Container, preferred: Boolean) = {
    var targetWidth = target.getSize.width
    if (targetWidth == 0) targetWidth = defaultWidth
    val hgap = getHgap
    val vgap = getVgap
    val insets = target.getInsets
    val horizontalInsetsAndGap = insets.left + insets.right + (hgap * 2)
    val maxWidth = targetWidth - horizontalInsetsAndGap
    val dim = new Dimension(0, 0)
    var rowWidth = 0
    var rowHeight = 0
    target.getComponents.foreach {m =>
      if (m.isVisible) {
        val d = if (preferred) m.getPreferredSize else m.getMinimumSize
        if (rowWidth + d.width > maxWidth) {
          addRow(dim, rowWidth, rowHeight)
          rowWidth = 0
          rowHeight = 0
        } else
          rowWidth += hgap
        rowWidth += d.width
        rowHeight = Math.max(rowHeight, d.height)
      }
    }
    addRow(dim, rowWidth, rowHeight)
    dim.width += horizontalInsetsAndGap
    dim.height += insets.top + insets.bottom + vgap * 2
    if (SwingUtilities.getAncestorOfClass(classOf[JScrollPane], target) != null && target.isValid)
      dim.width -= (hgap + 1)
    dim
  }

  private def addRow(dim: Dimension, rowWidth: Int, rowHeight: Int): Unit = {
    dim.width = Math.max(dim.width, rowWidth)
    if (dim.height > 0)
      dim.height += getVgap
    dim.height += rowHeight
  }
}
