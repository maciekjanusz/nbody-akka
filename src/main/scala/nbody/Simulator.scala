package nbody

import java.util.concurrent.TimeUnit

import akka.actor.SupervisorStrategy.{Resume, Restart, Stop, Escalate}
import akka.actor._

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag
import scala.util.Random

/*
  - skalowanie z realnym obciążeniem obliczeniowym
  - porównać z programem na pętli
  - sprawdzic czy da sie skonfigurowac ilosc wątków
  - oblozenie rdzeni

  - dopisac program z problemem szukania substringow
 */


case class ActorRefSeq(list: Seq[ActorRef])

case class StartSimulation(handler: ActorRef)

case class Finished[S <: State](result: Seq[S])

case object Ready

case object Start

trait State {
  def t: Long
}

class StateM2D(time: Long, val m: Double, val p: Point) extends State {
  override def t: Long = time
}

object StateM2D {
  def random(random: Random) =
    new StateM2D(0, random.nextDouble(), Point.random(random))
}

object Simulator {

  val G = 6.67408e-11

  val actorSystem = ActorSystem()
  actorSystem.registerOnTermination({
    System.exit(0)
  })

  val initialStates = StateSet.empty[StateM2D]

  def force(s1: StateM2D, s2: StateM2D): Double = G * ((s1.m * s2.m) / Point.distanceSq(s1.p, s2.p))

  def nextState(state: StateM2D, states: Seq[StateM2D]) = {
    val t = state.t + 1
    val random = new Random()
    val resForce = states.map {s2 => force(state, s2)}.sum
    states foreach {
      _ => 0 until 20 foreach {
        _ => random.nextDouble()
      }
    }

    new StateM2D(t, state.m, state.p)
  }

  def main(args: Array[String]): Unit = {
    //    println("java -jar <name> <l/a> <t> <n>")
    val tMax = if (args.nonEmpty) args(1).toLong else 1
    val actors = if (args.nonEmpty) args(2).toInt else 10000

    val random = new Random()
    0 until actors foreach {
      n => initialStates += StateM2D.random(random)
    }

    if (args.nonEmpty && (args(0) equals "l")) {
      val runner = new LoopSimulationRunner[StateM2D](tMax, initialStates, nextState)
      runner.run()
    } else {
      val bodySystem = actorSystem.actorOf(Props(classOf[BodySystem[StateM2D]],
        tMax, initialStates, nextState _,
        implicitly[ClassTag[StateM2D]]), name = "body-system")
      val handler = actorSystem.actorOf(Props[FinishedHandler], name = "finish-handler")
      bodySystem ! StartSimulation(handler)
    }
  }

}

class FinishedHandler extends Actor with ActorLogging {

  override def receive: Receive = {
    case Finished(results) =>
      context.stop(self)
      context.system.terminate()
  }

  override val supervisorStrategy =
    OneForOneStrategy() {
      case _: ArithmeticException      => Resume
      case _: NullPointerException     => Restart
      case _: IllegalArgumentException => Stop
      case _: Exception                => Escalate
    }
}