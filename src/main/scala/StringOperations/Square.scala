package StringOperations

import scala.collection.immutable.Stream.#::

/**
  * Created by Robert-PC on 9/25/2017.
  */
trait Square extends OperationFactory{
  lazy val squares: Stream[Number] = Pos("0") #:: Pos("1") #::
      //compute(Some(squares.last._1), Add, Some(squares.last._1)),
    compute(
      compute(
        Some(squares.last),
        Add,
        Some(squares.last)),
      Multiply,
      compute(
        Some(squares.last),
        Add,
        Some(squares.last)
      )
    ).getOrElse(Pos("0")) #:: Stream.empty

  val test: Stream[Number] = Pos("0") #:: Pos("1") #:: Pos("2") #:: Stream.empty
}
