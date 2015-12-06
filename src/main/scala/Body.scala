import akka.actor.{ActorRef, ActorLogging, Actor}

class Body(params: BodyParameters)
          (implicit systemParams: SystemParameters) extends Actor with ActorLogging {

  var states = Seq.empty[BodyState]
  states :+= BodyState(Point.random, 0)

  var peers = Seq.empty[ActorRef]
  var peerStates = Seq.empty[BodyState]
  var futureStates = Seq.empty[BodyState]

  override def receive: Receive = {
    case ActorRefSeq(list) =>
      peers = list
      sender ! Ready
      context.become(ready)
  }

  def ready: Receive = {
    case Start =>
      broadcastState(states.last)

    case obj: BodyState =>
      receiveState(obj)
  }

  def finished: Receive = {
    case _ =>
  }

  def receiveState(state: BodyState) {
    import state._
    val currentState = states.last

    if (t == currentState.t) {
      peerStates :+= state
    } else {//if (t > currentState.t) { // <-- nie ma mozliwosci, zeby bylo mniejsze
      futureStates :+= state
    }

    if (peerStates.size == peers.size) {
      // calculate new state
      val nextTime = t + 1
      val newState = BodyState.random(nextTime)  // <--- random bo w sumie nie wazne jak sie to liczy
      states :+= newState
      // remove all peer states
      peerStates = Seq.empty[BodyState]
      // broadcast new state
      broadcastState(newState)

      // if there are future states from other actors in extraStates,
      // handle them as if they had just been received
      if (futureStates.nonEmpty) {
        val nextStepStates = futureStates filter { es => es.t == nextTime } // <-- tu chyba mozna zalozyc, ze zawsze tak jest
        futureStates = futureStates diff nextStepStates
        nextStepStates foreach { state => receiveState(state) }
      }

      // finally, if it's the last iteration, finish
      if(nextTime == systemParams.t) finish()
    }
  }

  def finish() {
    context.parent ! Finished
    context.become(finished)
  }

  def broadcastState(state: BodyState) {
    peers foreach {
      peer => {
        peer ! state
      }
    }
  }

}