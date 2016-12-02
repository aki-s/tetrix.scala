package com.eed3si9n.tetrix

import com.aki.util.LoggerManager
import com.eed3si9n.util.Profiler
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

  val devlog = LoggerManager.dev
  /** Best move based on score.
    *
    * - score is calculated for all possible (action ++ Drop)
    * @param s0
    * @return `Tick` if no best move is found
    */
  def bestMove(s0: GameState): StageMessage = {
    var ret: Seq[StageMessage] = Nil
    var score: Double = MinUtility
    Profiler.stopWatch("bestMove") {
      val nodes = actionSeqs(s0) map { seq =>
        val act = seq ++ Seq(Drop)
        val s1 = Function.chain(act map {
          Stage.toTrans
        })(s0)
        val u = utility(s1)
        if (u > score) {
          score = u
          ret = seq
        }
        devlog.debug(s"1st score $score for strategy $ret.")
        SearchNode(s1, act, u)
      }
      nodes foreach { n =>
        actionSeqs(n.state) foreach { seq =>
          val act = seq ++ Seq(Drop)
          val s1 = Function.chain(act map {
            Stage.toTrans
          })(n.state)
          val u = utility(s1)
          if (u > score) {
            score = u
            ret = n.actions ++ seq
          }
        }
      }
      devlog.debug(s"2nd score $score for strategy $ret.")
    }
    ret.headOption getOrElse Tick
  }

  case class SearchNode(state: GameState, actions: Seq[StageMessage], score: Double)

  /** Count how many times current piece can be slide to.
    *
    * @param s0 GameState
    * @return (Left count, Right count). (0, 0) for s0.kind.Dummy.
    */
  private[tetrix] def sideLimit(s0: GameState): (Int, Int) = {
    /** Cannot stop sliding for `Dummy` kind. */
    @tailrec
    def leftLimit(n: Int, s: GameState): Int = {
      val next = Stage.moveLeft(s)
      if (next.currentPiece.pos == s.currentPiece.pos) n
      else leftLimit(n + 1, next)
    }
    /** Cannot stop sliding for `Dummy` kind. */
    @tailrec
    def rightLimit(n: Int, s: GameState): Int = {
      val next = Stage.moveRight(s)
      if (next.currentPiece.pos == s.currentPiece.pos) n
      else rightLimit(n + 1, next)
    }

    if (s0.currentPiece.kind == Dummy) (0, 0)
    else (leftLimit(0, s0), rightLimit(0, s0))
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
