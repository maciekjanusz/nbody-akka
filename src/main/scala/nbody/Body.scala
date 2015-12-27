package nbody

import akka.actor.{Actor, ActorLogging, ActorRef}

import scala.reflect.ClassTag

sealed class Body[S <: BodyState : ClassTag](tMax: Long, initialState: S, nextState: (S, Seq[S]) => S)
  extends Actor with ActorLogging {

  var currentState = initialState
  val peers = StateSet.empty[ActorRef]
  val peerStates = StateSet.empty[S]
  val futureStates = StateSet.empty[S]

  override def receive: Receive = {
    case ActorRefSeq(list) =>
      peers ++= list
      broadcastState(initialState)

    case state: S =>
      receiveState(state)
  }

  def receiveState(state: S): Unit = {
    if (state.t == currentState.t) {
      peerStates += state
    } else {
      futureStates += state
    }

    var t = state.t

    while (peerStates.size == peers.size) {
      // calculate new state
      val nextTime = t + 1
      currentState = nextState(currentState, peerStates)
      peerStates.clear()

      if(nextTime == tMax) {
        context.parent ! currentState
      } else {
        broadcastState(currentState)
        peerStates ++= futureStates
        futureStates.clear()

        t += 1
      }
    }
  }

  def broadcastState(state: BodyState): Unit = {
    peers foreach { _ ! state }
  }
}