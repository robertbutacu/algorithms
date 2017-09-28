package stringOperations.utils

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
case object Increment extends Operation
case object Decrement extends Operation
case object Square    extends Operation

trait Sign

case object NegativeLeftOperand     extends Sign
case object NegativeRightOperand    extends Sign
case object BothOperandsNegative    extends Sign
case object NoNegativeOperands      extends Sign
case object InvalidOperation        extends Sign

trait StringNumber extends Serializable{
  def number: String

  def apply(): String = number
}

case class Pos(number: String = "0") extends StringNumber
case class Neg(number: String = "0") extends StringNumber