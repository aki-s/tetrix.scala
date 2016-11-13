package com.eed3si9n.tetrix

class Agent {

  private[this] val minUtility = -1000.0

  def utility(state: GameState): Double =
    if (state.status == GameOver) -1000.0
    else state.lineCount.toDouble

  def bestMove(s0: GameState): StageMessage = {
    var ret: StageMessage = MoveLeft
    var current: Double = minUtility
    Stage.possibleMoves foreach { m =>
      val u = utility(Stage.toTrans(m)(s0))
      if (u > current) {
        current = u
        ret = m
      }
    }
    ret
  }

}
