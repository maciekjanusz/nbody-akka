package nbody

import java.util.concurrent.TimeUnit

import scala.reflect.ClassTag

class LoopSimulationRunner[S <: State : ClassTag](tMax: Long, initialStates: Seq[S], nextState: (S, Seq[S]) => S) {

  def run(): Unit = {

    val nanoStart = System.nanoTime()

    val time = 0L until tMax
    val states = StateSet.empty[S]
    val newStates = StateSet.empty[S]

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
    val deltaMillis = TimeUnit.NANOSECONDS.toMillis(delta)
    val avgDeltaMillis = deltaMillis / tMax
    println("Finished in " + deltaMillis + "ms (" + delta + "ns), avg " + avgDeltaMillis + "ms")
  }
}
