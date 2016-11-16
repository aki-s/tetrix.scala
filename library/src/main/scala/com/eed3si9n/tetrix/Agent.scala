package com.eed3si9n.tetrix

import com.typesafe.scalalogging.StrictLogging

import scala.annotation.tailrec

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

  /** Best move based on score.
    *
    * - score is calculated for all possible (action ++ Drop)
    * @param s0
    * @return `Tick` if no best move is found
    */
  def bestMove(s0: GameState): StageMessage = {
    var ret: Seq[StageMessage] = Nil
    var score: Double = MinUtility
    actionSeqs(s0) foreach { seq =>
      val act = seq ++ Seq(Drop)
      val u = utility(Function.chain(act map { Stage.toTrans })(s0))
      if (u > score) {
        score = u
        ret = seq
      }
    }
    logger.debug(s"Score $score for strategy $ret.")
    ret.headOption getOrElse Tick
  }

  /** Count how many times current piece can be slide to.
    *
    * @param s0
    * @return (Left count, Right count)
    */
  private[tetrix] def sideLimit(s0: GameState): (Int, Int) = {
    @tailrec
    def leftLimit(n: Int, s: GameState): Int = {
      val next = Stage.moveLeft(s)
      if (next.currentPiece.pos == s.currentPiece.pos) n
      else leftLimit(n + 1, next)
    }
    @tailrec
    def rightLimit(n: Int, s: GameState): Int = {
      val next = Stage.moveRight(s)
      if (next.currentPiece.pos == s.currentPiece.pos) n
      else rightLimit(n + 1, next)
    }
    (leftLimit(0, s0), rightLimit(0, s0))
  }

  /** Action pattern for Agent.
    *
    * @param s0
    * @return Seq of action pattern (== Seq of state applied possible action)
    */
  def actionSeqs(s0: GameState): Seq[Seq[StageMessage]] = {
    /** Seq(rotate_pattern) */
    val rotationSeqs: Seq[Seq[StageMessage]] = // Duplicate state for the same num of orientation.
      (0 until Piece.orientation(s0.currentPiece.kind)) map { Nil padTo (_, RotateCW)}

    /** Seq(slide_pattern) */
    val translationSeqs: Seq[Seq[StageMessage]] =
      sideLimit(s0) match { // s0 should be post-state of application of each rotationSeq
        case (l, r) => ((1 to l) map { Nil padTo (_, MoveLeft) }) ++
          Seq(Nil) ++ // No slide
          ((1 to r) map { Nil padTo (_, MoveRight) })
      }

    for {
      r <- rotationSeqs
      t <- translationSeqs
    } yield r ++ t
  }

}
