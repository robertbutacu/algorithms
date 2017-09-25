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

case object NegativeLeftOperand     extends Signum
case object NegativeRightOperand    extends Signum
case object BothOperandsNegative    extends Signum
case object NoNegativeOperands      extends Signum

trait Number{
  def number: String

  def apply() = number
}

case class Pos(number: String) extends Number
case class Neg(number: String) extends Number