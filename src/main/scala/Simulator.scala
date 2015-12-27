import akka.actor._

import scala.reflect.ClassTag

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

object Simulator extends App {

  val actorSystem = ActorSystem()
  actorSystem.registerOnTermination({
    System.exit(0)
  })

  def nextState(massBodyState: MassBodyState, states: Seq[MassBodyState]) = {
    val t = massBodyState.t + 1
    val mass = massBodyState.mass + 1
    MassBodyState(t, mass)
  }

  val tMax = 1L
  val actors = 6000

  var initialStates = Seq.empty[MassBodyState]
  0 until actors foreach {
    n => initialStates :+= new MassBodyState(0, 1)
  }

  val bodySystem = actorSystem.actorOf(Props(classOf[BodySystem[MassBodyState]],
    tMax, initialStates, nextState _,
    implicitly[ClassTag[MassBodyState]]), name = "system")

  val handler = actorSystem.actorOf(Props[FinishedHandler], name = "finish-handler")

  bodySystem ! StartSimulation(handler)

}

class FinishedHandler extends Actor with ActorLogging {

  override def receive: Receive = {
    case Finished(results) =>
      log.info("Finished (" + results.size + " bodies)")
      context.stop(self)
      context.system.terminate()
  }
}


//case class Point(x: Double, y: Double)
//object Point {
//  def random: Point = {
//    val random = new Random
//    apply(random.nextDouble(), random.nextDouble())
//  }
//}
