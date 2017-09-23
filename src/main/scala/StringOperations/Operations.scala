package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait Operation

case object Add      extends Operation
case object Subtract extends Operation
case object Multiply extends Operation
case object Divide   extends Operation
case object Pow      extends Operation

trait Signum

case object LeftOperandIsNegative   extends Signum
case object RightOperandIsNegative  extends Signum
case object BothOperandsAreNegative extends Signum
case object NoNegativeOperands      extends Signum