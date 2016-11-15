package com.eed3si9n.tetrix

import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

class AgentSpec extends Specification {
  override def is: SpecStructure =
    s2"""
      |
      | This is a specification to check Agent
      |
      | Utility function should
      |   evaluate initial state as 0.0,         $utility1
      |   evaluate GameOver as -1000.0,          $utility2
      |   evaluate an active state by lineCount  $utility3
      |   penalize having gaps between the columns !utility4^ // Other Idea: Implement penalty no blank space between blocks?
      |
      | Solver should
      |   pick MoveLeft for s1                   $solver1
      |   pick Drop for s3                       $solver2
      |
      |  Penalty function should
      |    penalize having blocks stacked up high $penalty1
      |
      |  ActionSeqs function should
      |    list out potential action sequences   $actionSeqs1
      |
    """.stripMargin

  val agent = new Agent
  val st1 = Stage.newState(Nil, view.Size, Stage.randomStream(new scala.util.Random))

  def utility1 =
    agent.utility(st1) must_== 0.0
  def utility2 =
    agent.utility(st1.copy(status = GameOver)) must_== -1000.0

  val rowToBeFilled = Seq((0, 0), (1, 0), (2, 0), (3, 0), (6, 0), (7, 0), (8, 0), (9, 0)) map { Block(_, Dummy) }
  /** State that just dropping the current piece clears a row. */
  val st3 = Stage.newState(rowToBeFilled, view.Size, Seq(OKind, Dummy, Dummy))
  def utility3 = {
    val s = Function.chain(Nil padTo (20, Stage.tick))(st3)
    agent.utility(s) must_== -1.0
  }
  def utility4 = {
    val s = Stage.newState(Seq(
      (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5))
      map { Block(_, TKind) }, (10, 20), TKind :: TKind :: Nil)
    agent.utility(s) must_== -36.0
    true
  } and {
    val s = Stage.newState(Seq((1, 0), (1, 1), (2, 1), (2, 2))
      map { Block(_, ZKind) }, (10, 20), TKind :: TKind :: Nil)
    agent.utility(s) must_== -14.0
  }

  def solver1 =
    agent.bestMove(st1) must_== MoveLeft
  def solver2 =
    agent.bestMove(st3) must_== Drop

  def penalty1 = {
    val s = Stage.newState(Seq(
      (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6))
      map { Block(_, Dummy) }, (10, 20), Dummy:: Dummy:: Nil)
    agent.penalty(s) must_== 49.0
  } and {
    val s = Stage.newState(Seq((1, 0))
      map { Block(_, Dummy) }, (10, 20), Dummy :: Dummy :: Nil)
    agent.penalty(s) must_== 1.0
  }

  def actionSeqs1 = {
    val s = Stage.newState(Nil, (10, 20), TKind :: TKind :: Nil)
    val seqs = agent.actionSeqs(s)
    seqs.size must_== 32 /** (width - width_of_TKind + 1) * rotate_patterns = 8*4 .. quasi-size
      * Width should be calculated for each rotate_pattern.
      */
  }

}
