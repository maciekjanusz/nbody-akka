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


case class ActorRefSeq(list: Seq[ActorRef])

case class StartSimulation(handler: ActorRef)

case class Finished[S <: BodyState](result: Seq[S])

case object Ready

case object Start

trait BodyState {
  def t: Long
}

case class MassBodyState(time: Long, val mass: Double) extends BodyState {
  override def t: Long = time
}

object Simulator {

  def main(args: Array[String]) {
    println("java -jar <name> <l/a> <t> <n>")

    val tMax = if (args.nonEmpty) args(1).toLong else 1
    val actors = if (args.nonEmpty) args(2).toInt else 10000

    def nextState(massBodyState: MassBodyState, states: Seq[MassBodyState]) = {
      val t = massBodyState.t + 1
      val mass = massBodyState.mass + 1
      val random = new Random()

      var i = 0
      val max = 1000
      while (i < max) {
        random.nextGaussian()
        i += 1
      }

      MassBodyState(t, mass)
    }

    val initialStates = StateSet.empty[MassBodyState]
    0 until actors foreach {
      n => initialStates += new MassBodyState(0, 1)
    }

    if (args.nonEmpty && (args(0) equals "l")) {
      val runner = new LoopSimulationRunner[MassBodyState](tMax, initialStates, nextState)
      runner.run()

    } else {
      val actorSystem = ActorSystem()

      actorSystem.registerOnTermination({
        System.exit(0)
      })

      val bodySystem = actorSystem.actorOf(Props(classOf[BodySystem[MassBodyState]],
        tMax, initialStates, nextState _,
        implicitly[ClassTag[MassBodyState]]), name = "system")
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
