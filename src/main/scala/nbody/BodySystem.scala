package nbody

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.reflect.ClassTag

class BodySystem[S <: State : ClassTag](tMax: Long, initialStates: Seq[S], nextState: (S, Seq[S]) => S)
  extends Actor with ActorLogging {

  val results = StateSet.empty[S]
  val iterator = Iterator.from(0)
  var nanoStart: Long = 0

  def n: Long = initialStates.size

  def startSimulation(): Unit = {
    nanoStart = System.nanoTime()

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
        val delta = System.nanoTime() - nanoStart
        val deltaMillis = TimeUnit.NANOSECONDS.toMillis(delta)
        val avgDeltaMillis = deltaMillis / tMax
        log.info("Finished in " + deltaMillis + "ms (" + delta + "ns), avg " + avgDeltaMillis + "ms")

        context.stop(self)
        handler ! Finished(results)
      }
  }
}

case class StartSimulation(handler: ActorRef)

case class Finished[S <: State](result: Seq[S])

case object Start