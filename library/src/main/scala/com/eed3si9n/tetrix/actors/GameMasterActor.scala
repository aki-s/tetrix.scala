package com.eed3si9n.tetrix.actors

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import com.eed3si9n.tetrix.{GameOver, GameState, GetState, Tick}

import scala.concurrent.Await
import scala.concurrent.duration._

class GameMasterActor(stateActor: ActorRef, agentActor: ActorRef) extends Actor {
  override def receive: Receive = {
    case Tick =>
      val s = getState
      if (s.status != GameOver) {
        agentActor ! BestMove(getState)
      }
  }

  private[this] def getState: GameState = {
    val future = (stateActor ? GetState)(1 second).mapTo[GameState]
    Await.result(future, 1 second)
  }

}
