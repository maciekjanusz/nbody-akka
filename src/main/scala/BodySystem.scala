import java.util.concurrent.TimeUnit

import akka.actor.{Props, ActorRef, ActorLogging, Actor}

import scala.reflect.ClassTag

final class BodySystem[S <: BodyState : ClassTag](tMax: Long, initialStates: Seq[S], nextState: (S, Seq[S]) => S)
    extends Actor with ActorLogging {

  var bodies = Seq.empty[ActorRef]
  var results = Seq.empty[S]
  var nReady = 0
  var nFinished = 0

  var nanoStart: Long = 0

  def n = initialStates.size

  def startSimulation(): Unit = {
    val iterator = Iterator.from(0)
    // spawn actors
    initialStates foreach { state =>
      bodies :+= context.watch {
        context.actorOf(Props(classOf[Body[S]], tMax, state, nextState, implicitly[ClassTag[S]]),
          name = "body-" + iterator.next())
      }
    }

    // send peer lists to actors
    bodies foreach { body =>
      body ! ActorRefSeq(bodies.filter { b => b != body })
    }
  }

  override def receive: Receive = {
    case StartSimulation(handler) =>
      log.info("Starting simulation...")
      context.become(started(handler))
      startSimulation()
  }

  def started(handler: ActorRef): Receive = {
    case Ready =>
      nReady += 1
      if(nReady == n) {
        nanoStart = System.nanoTime()
        bodies foreach { body =>
          body ! Start
        }
      }

    case state: S =>
      results :+= state
      if(results.size == n) {
        val delta = System.nanoTime() - nanoStart
        val avgFrame = delta / tMax
        val deltaMillis = TimeUnit.NANOSECONDS.toMillis(delta)
        val avgMillis = TimeUnit.NANOSECONDS.toMillis(avgFrame)

        context.stop(self)
        handler ! Finished(results)
        log.info("Finished in " + deltaMillis + " ms, avg t/frame = " + avgMillis + " ms")
      }
  }
}