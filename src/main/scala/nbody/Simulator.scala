package nbody

import akka.actor._

import scala.reflect.ClassTag
import scala.util.Random

class TimedState(timeStep: Long) extends State {
  def t = timeStep
}

object Simulator {

  val initialStates = StateSet.empty[TimedState]

  def nextState(state: TimedState, states: Seq[TimedState]) = {
    import state._
    val random = new Random()
    states foreach { _ =>
      0 until 20 foreach { _ =>
        random.nextDouble()
      }
    }
    new TimedState(t + 1)
  }

  def main(args: Array[String]): Unit = {
    val mode = args(0)
    val tMax = args(1).toLong
    val n = args(2).toInt

    println(mode + ", tMax = " + tMax + ", n = " + n)

    0 until n foreach {
      n => initialStates += new TimedState(0)
    }

    if (args.nonEmpty && (mode equals "l")) {
      val runner = new LoopSimulationRunner[TimedState](tMax, initialStates, nextState)
      runner.run()

    } else {
      val actorSystem = ActorSystem()
      actorSystem.registerOnTermination({
        System.exit(0)
      })
      val bodySystem = actorSystem.actorOf(Props(classOf[BodySystem[TimedState]],
        tMax, initialStates, nextState _,
        implicitly[ClassTag[TimedState]]), name = "body-system")
      val handler = actorSystem.actorOf(Props[FinishedHandler], name = "finish-handler")
      bodySystem ! StartSimulation(handler)
    }
  }
}

class FinishedHandler extends Actor with ActorLogging {

  override def receive: Receive = {
    case Finished(results) =>
      context.stop(self)
      log.info("S = " + results.last.t)
      context.system.terminate()
  }
}