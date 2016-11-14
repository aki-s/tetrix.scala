package com.eed3si9n.tetrix

class Agent {

  private[this] val MinUtility = -1000.0

  def utility(state: GameState): Double =
    if (state.status == GameOver) -1000.0
    else state.lineCount.toDouble - penalty(state)

  private[this] def penalty(s: GameState): Double = {
    val heights = s.unload(s.currentPiece).blocks map { _.pos } groupBy {
      _._1 } map { case (k, v) => (k, v.size) }
    val gaps = (0 to s.gridSize._1 - 2) map { x =>
      heights.getOrElse(x, 0) - heights.getOrElse(x + 1, 0)
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
