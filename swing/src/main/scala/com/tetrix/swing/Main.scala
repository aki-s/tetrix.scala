package com.tetrix.swing

import java.awt.event.ActionEvent
import java.awt.{Dimension, Graphics2D, Rectangle, Color => AWTColor}
import javax.swing.AbstractAction

import com.eed3si9n.tetrix.view.{Blocks, Grid}
import com.eed3si9n.tetrix._
import com.typesafe.scalalogging.Logger

import scala.swing.event.Key.{Down, Left, Right, Space, Up, Value}
import scala.swing.event.KeyPressed
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}

object Main extends SimpleSwingApplication {
  val logger = Logger("Main")

  val bluishGray = new AWTColor(48, 99, 99) // for canvas
  val bluishLigherGray =  new AWTColor(96, 189, 255) // for mesh
  val bluishEvenLigher = new AWTColor(192, 200, 200) // for block
  val bluishSilver = new AWTColor(210, 255, 255) // for current block
  val blockSize = 16
  val blockMargin = 2
  val blockUnit = blockSize + blockMargin
  val mainPanelSize = new Dimension(700, 400)

  val ui = new AbstractUI

  def onKeyPress(keyCode: Value) = keyCode match {
    case Left => ui.left()
    case Right => ui.right()
    case Up => ui.up()
    case Down => ui.down()
    case Space => ui.space()
    case a@_ => // do something
      println(a)
  }

  def onPaint(g: Graphics2D): Unit = {
    val view1 = ui.stView
    drawBoard(g, (blockUnit, 0), view1.gridSize, view1.blocks, view1.current.map(_.pos))
    logger.debug(view1.toString)

    val offset = ((view.Size._1 + 2) * blockUnit, 0)
    drawBoard(g, offset, view.MiniGridSize, Nil,
      view1.next.head.kind.basicPos)

    // Plot Y label
    for { y <- 0 until view1.gridSize._2 } {
      val (yX, yY) = newAxis((0, blockUnit/2), view1.gridSize, (0, y))
      g.drawString(s"$y", yX, yY)
    }
  }

  /**
    *
    * @param g
    * @param offset upper left corner for grid in AWT coordinate
    * @param gridSize (x,y) size in block coordinate
    * @param allBlocks in unit of block (block coordinate)
    * @param currentBlocks block position in each block coordinate
    */
  def drawBoard(g: Graphics2D, offset: Grid, gridSize: Grid, allBlocks: Blocks, currentBlocks: Seq[Grid]): Unit = {

    /** Reverse y-axis. Convert to absolute coordinate of AWT. */
    def buildRect(pos: Grid): Rectangle = {
      val (x, y) = newAxis(offset, gridSize, pos)

      new Rectangle(x, y, blockSize, blockSize)
    }

    def drawEmptyGrid(): Unit = {
      g setColor bluishLigherGray
      for {
        x <- 0 until gridSize._1
        y <- 0 until gridSize._2
      } {
        val pos = (x, y)
        g draw buildRect(pos)
      }
    }
    def drawBlocks(): Unit = {
      g setColor bluishEvenLigher
      allBlocks foreach { b => g fill buildRect(b.pos) }
    }
    def drawCurrent(): Unit = {
      g setColor bluishSilver
      currentBlocks foreach { b => g fill buildRect(b) }
    }

    drawEmptyGrid()
    drawBlocks()
    drawCurrent()
  }
  private def newAxis(offset: Grid, gridSize: Grid, pos: Grid): (Int, Int) =
    (offset._1 + pos._1 * blockUnit,
      offset._2 + (gridSize._2 - pos._2) * blockUnit)

  def top = new MainFrame {
    title = "tetrix"
    contents = mainPanel
  }

  def mainPanel = new Panel {
    preferredSize = mainPanelSize
    focusable = true
    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) =>
        onKeyPress(key)
        repaint
    }
    override def paint(g: Graphics2D): Unit = {
      g setColor bluishGray
      // + ->x
      // ↓
      // y
      g fillRect (0, 0, size.width, size.height)
      onPaint(g)
    }
    val timer = new javax.swing.Timer(view.FRate, new AbstractAction() {
      override def actionPerformed(e: ActionEvent): Unit = repaint
    })
    timer.start()
  }

}
