package stringOperations

import stringOperations.utils.{Add, Multiply, Pos, Subtract}

import scala.collection.immutable.Stream.#::

/**
  * Created by Robert-PC on 9/25/2017.
  */
trait StreamsExamples extends OperationFactory{
  lazy val squares: Stream[utils.StringNumber] = Pos("1") #:: Pos("2") #::
      //compute(Some(squares.last._1), Add, Some(squares.last._1)),
    compute(
      compute(
        Some(squares.tail.last),
        Add,
        Some(squares.tail.last)),
      Multiply,
      compute(
        Some(squares.tail.last),
        Add,
        Some(squares.tail.last)
      )
    ).get #:: Stream.empty

  def squaresUpUntil(i: utils.StringNumber): Stream[utils.StringNumber] = {
    if (i == Pos())
      Stream.empty
    else
      Stream.cons(
        compute(Some(i), Multiply, Some(i)).get,
        squaresUpUntil(
          compute(Some(i), Subtract, Some(Pos("1"))).get
        )
      )
  }

  lazy val fibsString: Stream[utils.StringNumber] = Pos("0") #::
    Pos("1") #::
    fibsString.zip(fibsString.tail).map { e =>
      compute(
        Some(e._1),
        Add,
        Some(e._2)
      ).get
    }

  val test: Stream[utils.StringNumber] = Pos("0") #:: Pos("1") #:: Pos("2") #:: Stream.empty
}
