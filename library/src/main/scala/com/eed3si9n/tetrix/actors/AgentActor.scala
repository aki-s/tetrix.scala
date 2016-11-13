package com.eed3si9n.tetrix.actors

import akka.actor.{Actor, ActorRef}
import com.eed3si9n.tetrix.{Agent, GameState}
import com.typesafe.scalalogging.StrictLogging

sealed trait AgentMessage
case class BestMove(s: GameState) extends AgentMessage

class AgentActor(stageActor: ActorRef) extends Actor with StrictLogging {
  private[this] val agent = new Agent

  override def receive: Receive = {
    case BestMove(s: GameState) =>
      val message = agent.bestMove(s)
      logger.debug("selected " + message)
      stageActor ! message
  }
}

