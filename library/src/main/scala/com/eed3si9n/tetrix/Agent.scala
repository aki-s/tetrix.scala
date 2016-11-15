package com.eed3si9n.tetrix

import com.typesafe.scalalogging.StrictLogging

class Agent extends StrictLogging {

  private[this] val MinUtility = -1000.0

  /** Calculate score.
    *
    * The greater score is, the better decision is.
    */
  def utility(state: GameState): Double =
    if (state.status == GameOver) MinUtility
    else reward(state) - penalty(state)

  private[tetrix] def reward(s: GameState): Double = {
    s.lineCount.toDouble
  }

  /** {{{ Penalty := sum_i |height_i|^2 }}} */
  private[tetrix] def penalty(s: GameState): Double = {
    /** 1-based-height for each column. */
    val heights = s.unload(s.currentPiece).blocks map { _.pos } groupBy {
      _._1 } map { case (k, v) => v.map {_._2 + 1} max }
    heights map { x => x * x } sum
  }

  /** Best move based on score. */
  def bestMove(s0: GameState): StageMessage = {
    var ret: StageMessage = MoveLeft
    var score: Double = MinUtility
    Stage.possibleMoves foreach { m =>
      val u = utility(Stage.toTrans(m)(s0))
      if (u > score) {
        score = u
        ret = m
      }
    }
    logger.debug(s"Score $score for strategy $ret.")
    ret
  }

}
