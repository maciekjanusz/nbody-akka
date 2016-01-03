package object nbody {

  type StateSet[T] = scala.collection.mutable.UnrolledBuffer[T]
  val StateSet = scala.collection.mutable.UnrolledBuffer
}
