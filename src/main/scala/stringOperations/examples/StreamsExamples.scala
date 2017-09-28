package stringOperations.examples

import stringOperations.utils._
import stringOperations.{OperationFactory, utils}

/**
  * Created by Robert-PC on 9/25/2017.
  */
trait StreamsExamples extends OperationFactory{
  //TODO
  lazy val squares: Stream[utils.StringNumber] = Stream.empty

  def squaresUpUntil(i: StringNumber): Stream[StringNumber] = {
    if (i == Pos())
      Stream.empty
    else
      Stream.cons(
        Pos(compute(Square, Some(i))),
        squaresUpUntil(
          Pos(compute(Decrement, Some(i)))
        )
      )
  }

  def squaresBetween(start: StringNumber, end: StringNumber): Stream[StringNumber] = {
    if (start == end)
      Stream.empty
    else
      Stream.cons(
        compute(Some(start), Multiply, Some(start)).get,
        squaresBetween(
          compute(Some(start), Add, Some(Pos("1"))).get, end
        )
      )
    }

  lazy val fibsString: Stream[StringNumber] = Pos() #::
    Pos("1") #::
    fibsString.zip(fibsString.tail).map { e =>
      compute(
        Some(e._1),
        Add,
        Some(e._2)
      ).get
    }
}
