package com.eed3si9n.tetrix.actors

import akka.actor.{Actor, ActorRef}
import com.eed3si9n.tetrix.{Agent, Drop, GameState}
import com.eed3si9n.util.Profiler

sealed trait AgentMessage
case class BestMove(s: GameState) extends AgentMessage

class AgentActor(stageActor: ActorRef) extends Actor {
  private[this] val agent = new Agent

  override def receive: Receive = {
    case BestMove(s: GameState) =>
      val message = Profiler.stopWatch() { (agent.bestMove(s)) }
      if (message ne Drop) stageActor ! message
  }
}

