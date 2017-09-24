package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
object Division {
  private[StringOperations] def apply(x: String, y: String) = {
    def divide(x: String, y: String) = {}

    x.zip(y)
      .foldRight(Total())((curr, acc) =>
        Total(

        )
      ).total
  }
}
