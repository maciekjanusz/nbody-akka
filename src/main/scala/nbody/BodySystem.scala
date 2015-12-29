package nbody

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.reflect.ClassTag

class BodySystem[S <: State : ClassTag](tMax: Long, initialStates: Seq[S], nextState: (S, Seq[S]) => S)
    extends Actor with ActorLogging {

  val bodies = StateSet.empty[ActorRef]
  val results = StateSet.empty[S]

  var nanoStart: Long = 0

  def n: Long = initialStates.size

  def startSimulation(): Unit = {
    val iterator = Iterator.from(0)
    // spawn actors
    initialStates foreach { state =>
      bodies += context.watch {
        context.actorOf(Props(classOf[Body[S]], n, tMax, state, nextState, implicitly[ClassTag[S]]),
          name = "body-" + iterator.next())
      }
    }

    // send peer lists to actors
    nanoStart = System.nanoTime()

    bodies foreach { body =>
      body ! Start
    }
  }

  override def receive: Receive = {
    case StartSimulation(handler) =>
      log.info("Starting simulation...")
      context.become(started(handler))
      startSimulation()
  }

  def started(handler: ActorRef): Receive = {
    case state: S =>
      results += state
      if(results.size == n) {
        val delta = System.nanoTime() - nanoStart
        val avgFrame = delta / tMax
        val deltaMillis = TimeUnit.NANOSECONDS.toMillis(delta)
        val avgMillis = TimeUnit.NANOSECONDS.toMillis(avgFrame)

        context.stop(self)
        handler ! Finished(results)
        log.info("Finished in " + deltaMillis + " ms, avg t/frame = " + avgMillis + " ms (" + avgFrame + "ns)")
      }
  }
}