import akka.actor.{ActorRef, ActorLogging, Actor}

import scala.collection.mutable
import scala.reflect.ClassTag

sealed class Body[S <: BodyState : ClassTag](tMax: Long, initialState: S, nextState: (S, Seq[S]) => S)
  extends Actor with ActorLogging {

  var currentState = initialState
  val peers = mutable.Buffer.empty[ActorRef]
  val peerStates = mutable.Buffer.empty[S]
  val futureStates = mutable.Buffer.empty[S]

  override def receive: Receive = {
    case ActorRefSeq(list) =>
      peers ++= list
      sender ! Ready
      context.become(ready)
  }

  def ready: Receive = {
    case Start =>
      broadcastState(initialState)

    case state: S =>
      receiveState(state)
  }

  def finished: Receive = {
    case _ =>
  }

  def receiveState(state: S): Unit = {
    if (state.t == currentState.t) {
      peerStates += state
    } else {
      futureStates += state
    }

    while (peerStates.size == peers.size) {
      print(". ")
      // calculate new state
      val nextTime = state.t + 1
      val newState = nextState(currentState, peerStates)
      currentState = newState
      peerStates.clear()
      broadcastState(newState)

      peerStates ++= futureStates
      futureStates.clear()

      if(nextTime == tMax) finish()
    }
  }

  def finish(): Unit = {
    context.parent ! currentState
    context.become(finished)
  }

  def broadcastState(state: BodyState): Unit = {
    peers foreach { _ ! state }
  }
}