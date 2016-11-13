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
      |
    """.stripMargin

  val agent = new Agent
  val st1 = Stage.newState(Nil, view.Size, Stage.randomStream(new scala.util.Random))

  def utility1 =
    agent.utility(st1) must_== 0.0
  def utility2 =
    agent.utility(st1.copy(status = GameOver)) must_== -1000.0

  def utility3 = {
    val rowToBeFilled = Seq((0, 0), (1, 0), (2, 0), (3, 0), (6, 0), (7, 0), (8, 0), (9, 0)) map { Block(_, Dummy) }
    val s = Function.chain(Nil padTo (20, Stage.tick))(Stage.newState(rowToBeFilled, view.Size, Seq(OKind, Dummy, Dummy)))
    agent.utility(s) must_== 1.0
  }
}
