package nbody

import akka.actor.{Actor, ActorLogging}

import scala.reflect.ClassTag

class Body[S <: State : ClassTag](n: Long, tMax: Long, initialState: S, nextState: (S, Seq[S]) => S)
  extends Actor with ActorLogging {

  var started = false
  val peers = n - 1
  val bodies = context.actorSelection("../*")
  val peerStates = StateSet.empty[S]
  val futureStates = StateSet.empty[S]
  var currentState = initialState

  override def receive: Receive = {
    case Start =>
      if (!started) {
        broadcastState(initialState)
        started = true
      }

    case state: S =>
      if (sender != self) {
        receiveState(state)
      }
  }

  def receiveState(state: S): Unit = {
    if (state.t == currentState.t) {
      peerStates += state
    } else {
      futureStates += state
    }

    var t = state.t
    while (peerStates.size == peers) {
      currentState = nextState(currentState, peerStates)
      peerStates.clear()

      if (t + 1 == tMax) {
        context.parent ! currentState
      } else {
        broadcastState(currentState)
        peerStates ++= futureStates
        futureStates.clear()

        t += 1
      }
    }
  }

  def broadcastState(state: S): Unit = {
    bodies ! state
  }
}