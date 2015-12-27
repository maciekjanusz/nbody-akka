import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.reflect.ClassTag

object LoopSimulator extends App {

  def nextState(massBodyState: MassBodyState, states: Seq[MassBodyState]) = {

    val t = massBodyState.t + 1
    val mass = massBodyState.mass + 1
    MassBodyState(t, mass)
  }

  val tMax = 1000L
  val actors = 10

  var initialStates = Seq.empty[MassBodyState]
  0 until actors foreach {
    n => initialStates :+= new MassBodyState(0, 1)
  }

  val runner = new LoopSimulationRunner[MassBodyState](tMax, initialStates, nextState)

  val tries = 10
  0 until tries foreach {
    n => runner.run()
  }
}

class LoopSimulationRunner[S <: BodyState : ClassTag](tMax: Long, initialStates: Seq[S], nextState: (S, Seq[S]) => S) {

  def run(): Unit = {

    val nanoStart = System.nanoTime()

    val time = 0L until tMax
    val states = mutable.Buffer.empty[S]
    val newStates = mutable.Buffer.empty[S]

    states ++= initialStates

    time foreach { step =>
      states foreach { state =>
        val otherStates = states filter { s => s != state }
        newStates += nextState(state, otherStates)
      }
      states.clear()
      states ++= newStates
      newStates.clear()
    }

    val delta = System.nanoTime() - nanoStart
    val avgFrame = delta / tMax
    val deltaMillis = TimeUnit.NANOSECONDS.toMillis(delta)
    val avgMillis = TimeUnit.NANOSECONDS.toMillis(avgFrame)

    println("Finished in " + deltaMillis + " ms, avg t/frame = " + avgMillis + " ms (" + avgFrame + "ns)")
  }

}
