/**
  * Created by maciek-private on 27.12.2015.
  */
package object nbody {

  type StateSet[T] = scala.collection.mutable.UnrolledBuffer[T]
  val StateSet = scala.collection.mutable.UnrolledBuffer
}
