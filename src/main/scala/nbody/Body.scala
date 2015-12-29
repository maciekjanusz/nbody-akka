package nbody

import akka.actor.{Actor, ActorLogging, ActorRef}

import scala.reflect.ClassTag

sealed class Body[S <: BodyState : ClassTag](n: Long, tMax: Long, initialState: S, nextState: (S, Seq[S]) => S)
  extends Actor with ActorLogging {

  def peers = n - 1

  var currentState = initialState
  //  val peerStates = StateSet.empty[S]
  var processedStates = 0
  var futureStates = 0 //StateSet.empty[S]

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
      processedStates += 1
    } else {
      futureStates += 1
    }

    var t = state.t

    while (processedStates == peers) {
      if(named("body-0")) print("#")
      processedStates = 0
      val tPlus = t + 1
      currentState = nextState(currentState, Seq.empty[S])

      if (tPlus == tMax) {
        context.parent ! currentState
      } else {
        broadcastState(currentState)
        processedStates += futureStates
        futureStates = 0
        t += 1
      }
    }
  }

  def broadcastState(state: S): Unit = {
    context.actorSelection("../*") ! state
  }

  def named(name: String) = self.path.name equals name
}