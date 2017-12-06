package advent.shared

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

object Mutation {
  def mutate[A: ClassTag, M[A] <: TraversableOnce[A]](collection: M[A])(block: Array[A] => Unit)(
      implicit bf: CanBuildFrom[M[A], A, M[A]]): M[A] = {
    val array = collection.toArray
    block(array)
    val builder = bf()
    builder ++= array
    builder.result()
  }
}
