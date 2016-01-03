package nbody

import akka.actor._

import scala.reflect.ClassTag
import scala.util.Random

/*
  - skalowanie z realnym obciążeniem obliczeniowym
  - porównać z programem na pętli
  - sprawdzic czy da sie skonfigurowac ilosc wątków
  - oblozenie rdzeni

  - dopisac program z problemem szukania substringow
 */

class TimedState(timeStep: Long) {
  def t = timeStep
}

object Simulator {

  val actorSystem = ActorSystem()
  actorSystem.registerOnTermination({
    System.exit(0)
  })

  val initialStates = StateSet.empty[TimedState]

  def nextState(state: TimedState, states: Seq[TimedState]) = {
    import state._
    val random = new Random()
    states foreach {
      _ => 0 until 20 foreach {
        _ => random.nextDouble()
      }
    }
    new TimedState(t + 1)
  }

  def main(args: Array[String]): Unit = {
    //    println("java -jar <name> <l/a> <t> <n>")
    val tMax = args(1).toLong
    val actors = args(2).toInt

    val random = new Random()
    0 until actors foreach {
      n => initialStates += new TimedState(0)
    }

    if (args.nonEmpty && (args(0) equals "l")) {
      val runner = new LoopSimulationRunner[TimedState](tMax, initialStates, nextState)
      runner.run()
    } else {
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
      context.system.terminate()
  }
}