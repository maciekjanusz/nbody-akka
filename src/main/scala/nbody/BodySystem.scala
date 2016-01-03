package nbody

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.reflect.ClassTag

class BodySystem[S <: State : ClassTag](tMax: Long, initialStates: Seq[S], nextState: (S, Seq[S]) => S)
  extends Actor with ActorLogging {

  val results = StateSet.empty[S]

  var nanoStart: Long = 0

  def n: Long = initialStates.size

  def startSimulation(): Unit = {
    val iterator = Iterator.from(0)
    val bodies = StateSet.empty[ActorRef]

    initialStates foreach { state =>
      bodies += context.actorOf(Props(classOf[Body[S]],
        n, tMax, state, nextState, implicitly[ClassTag[S]]), name = "body-" + iterator.next())
    }
    bodies foreach { body =>
      body ! Start
    }
  }

  override def receive: Receive = {
    case StartSimulation(handler) =>
      context.become(started(handler))
      startSimulation()
  }

  def started(handler: ActorRef): Receive = {
    case state: S =>
      results += state
      if (results.size == n) {
        context.stop(self)
        handler ! Finished(results)
      }
  }
}

case class StartSimulation(handler: ActorRef)

case class Finished[S <: State](result: Seq[S])

case object Start