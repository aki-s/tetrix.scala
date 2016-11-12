package com.eed3si9n.tetrix

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask

import scala.concurrent.Await
import scala.concurrent.duration._

sealed trait StateMessage
case object GetState extends StateMessage
case class SetState(s: GameState) extends StateMessage
case object GetView extends StateMessage

class StateActor(s0: GameState) extends Actor {
  private[this] var state: GameState = s0

  override def receive: Receive = {
    case GetState => sender ! state
    case SetState(s) => state = s
    case GetView => sender ! state.view
  }

}

sealed trait StageMessage
case object MoveLeft extends StageMessage
case object MoveRight extends StageMessage
case object RotateCW extends StageMessage
case object Tick extends StageMessage
case object Drop extends StageMessage

class StageActor(stateActor: ActorRef) extends Actor {
  import Stage._

  override def receive: Receive = {
    case MoveLeft  => updateState { moveLeft }
    case MoveRight => updateState { moveRight }
    case RotateCW  => updateState { rotateCW }
    case Tick      => updateState { tick }
    case Drop      => updateState { drop }
  }

  private[this] def updateState(trans: GameState => GameState): Unit = {
    val future = (stateActor ? GetState)(4000 millis).mapTo[GameState]
    val s1 = Await.result(future, 2000 millis)
    val s2 = trans(s1)
    stateActor ! SetState(s2)
  }

}
