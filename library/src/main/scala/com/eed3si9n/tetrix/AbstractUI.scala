package com.eed3si9n.tetrix

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.eed3si9n.tetrix.Stage._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

class AbstractUI {
  private[this] val initialState: GameState = newState(Nil,
    view.Size, randomStream(new scala.util.Random))
  private[this] val system = ActorSystem("TetrixSystem")
  private[this] val playerActor = system.actorOf(Props(classOf[StageActor], initialState),
    name = "playerActor")
  private[this] val timer = system.scheduler.schedule(
    0 milliseconds, 1000 millisecond, playerActor, Tick)(ExecutionContext.Implicits.global)
  implicit val timeout1 = Timeout(1 second)
  implicit val timeout2 = Timeout(2 second)

  def left(): Unit = { playerActor ! MoveLeft }
  def right(): Unit = { playerActor ! MoveRight }
  def up(): Unit = { }
  def down(): Unit = { playerActor ! Drop }
  def space(): Unit = { playerActor ! RotateCW }
  def stView: GameView = Await.result(playerActor.ask(View)(timeout1).mapTo[GameView], timeout2.duration)
}
