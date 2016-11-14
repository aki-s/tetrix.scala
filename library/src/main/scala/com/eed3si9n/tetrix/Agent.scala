package com.eed3si9n.tetrix

class Agent {

  private[this] val MinUtility = -1000.0

  def utility(state: GameState): Double =
    if (state.status == GameOver) -1000.0
    else state.lineCount.toDouble - penalty(state)

  /** {{{ Penalty := sum_i |(diff of height of adjacent column)_i|^2 }}} */
  private[this] def penalty(s: GameState): Double = {
    // (x,y) = (x, 0-based-height)
    val heights = s.unload(s.currentPiece).blocks map { _.pos } groupBy {
      _._1 } map { case (k, v) => (k, v.map(_._2).max) }
    val gaps = (0 to s.gridSize._1 - 2) map { x =>
      heights.getOrElse(x, -1) - heights.getOrElse(x + 1, -1)
    } filter { math.abs(_) > 0 } // Intentionally `>0`, not `>1`
    gaps map { x => x * x } sum
  }

  def bestMove(s0: GameState): StageMessage = {
    var ret: StageMessage = MoveLeft
    var current: Double = MinUtility
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
