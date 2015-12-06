import akka.actor._

import scala.util.Random

object Simulator extends App {

  val params = SystemParameters(100, 1000, 0.1) // objects, time frames, time delta

  val bodySystem = ActorSystem().actorOf(Props(classOf[BodySystem], params), name = "system")
  bodySystem ! Start
}

case class SystemParameters(n: Int, t: Int, dt: Double)

case class BodyParameters(index: Int, mass: Double)

case class BodyState(position: Point, t: Int)
object BodyState {
  def random(t: Int): BodyState = {
    apply(Point.random, t)
  }
}

case class Point(x: Double, y: Double)
object Point {
  def random: Point = {
    val random = new Random
    apply(random.nextDouble(), random.nextDouble())
  }
}

case class ActorRefSeq(list: Seq[ActorRef])
case object Start
case object Ready
case object Finished
