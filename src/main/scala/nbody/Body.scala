package nbody

import akka.actor.{Actor, ActorLogging}

import scala.reflect.ClassTag

class Body[S <: State : ClassTag](n: Long, tMax: Long, initialState: S, nextState: (S, Seq[S]) => S)
  extends Actor with ActorLogging {

  def peers = n - 1

  var currentState = initialState
  val bodies = context.actorSelection("../*") // siblings
  val peerStates = StateSet.empty[S]
  val futureStates = StateSet.empty[S]

  override def receive: Receive = {
    case Start =>
      broadcastState(initialState)

    case state: S =>
      if (sender() != self) {
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
      // calculate new state
      val nextTime = t + 1
      currentState = nextState(currentState, peerStates)
      peerStates.clear()

      if (nextTime == tMax) {
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

  def named(name: String) = self.path.name equals name
}