package com.eed3si9n.tetrix

import java.{util => ju}
import Stage._

class AbstractUI {
  private[this] val initialState = newState(Nil,
    view.Size, randomStream(new scala.util.Random))
  private[this] var state = initialState
  private[this] val timer = new ju.Timer
  timer.scheduleAtFixedRate(new ju.TimerTask {
    override def run(): Unit = down()
  }, 0, view.FRate)

  def left(): Unit = {
    state = moveLeft(state)
  }
  def right(): Unit = {
    state = moveRight(state)
  }
  def up(): Unit = {
  }
  def down(): Unit = {
    state = tick(state)
  }
  def space(): Unit = {
    state = rotateCW(state)
  }

  def stView: GameView = state.view

}
