package nbody

import java.util.concurrent.TimeUnit

import scala.collection.mutable
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
    val avgFrame = delta / tMax
    val deltaMillis = TimeUnit.NANOSECONDS.toMillis(delta)
    val avgMillis = TimeUnit.NANOSECONDS.toMillis(avgFrame)

    println("Finished in " + deltaMillis + " ms, avg t/frame = " + avgMillis + " ms (" + avgFrame + "ns)")
  }

}
