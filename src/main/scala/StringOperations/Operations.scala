package StringOperations

/**
  * Created by Robert-PC on 9/21/2017.
  */
trait Operation

case object Add       extends Operation
case object Subtract  extends Operation
case object Multiply  extends Operation
case object Divide    extends Operation
case object Pow       extends Operation
case object Squared   extends Operation
case object Root      extends Operation
case object Modulus   extends Operation

trait Sign

case object NegativeLeftOperand     extends Sign
case object NegativeRightOperand    extends Sign
case object BothOperandsNegative    extends Sign
case object NoNegativeOperands      extends Sign
case object InvalidOperation        extends Sign

trait Number{
  def number: String

  def apply(): String = number
}

case class Pos(number: String) extends Number
case class Neg(number: String) extends Number