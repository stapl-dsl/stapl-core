package stapl.core

sealed abstract class Operation extends Value

case class Addition(left: Value, right: Value) extends Operation {

  override val aType =
    if (!left.isList && !right.isList)
      left.aType.addition(right.aType).getOrElse {
        val lType = left.aType
        val rType = right.aType
        throw new TypeCheckException(s"Cannot add a $rType to a $lType.")
      }
    else
      throw new UnsupportedOperationException("Cannot add lists.")
  
  override val isList = false
  
  override def getConcreteValue(ctx: EvaluationCtx): ConcreteValue = {
    val leftValue = left.getConcreteValue(ctx)
    val rightValue = right.getConcreteValue(ctx)
    
    leftValue.add(rightValue)
  }
}

case class Subtraction(left: Value, right: Value) extends Operation {

  override val aType =
    if (!left.isList && !right.isList)
      left.aType.subtraction(right.aType).getOrElse {
        val lType = left.aType
        val rType = right.aType
        throw new TypeCheckException(s"Cannot subtract a $rType from a $lType.")
      }
    else
      throw new UnsupportedOperationException("Cannot subtract lists.")
  
  override val isList = false
  
  override def getConcreteValue(ctx: EvaluationCtx): ConcreteValue = {
    val leftValue = left.getConcreteValue(ctx)
    val rightValue = right.getConcreteValue(ctx)
    
    leftValue.subtract(rightValue)
  }
}

case class Multiplication(left: Value, right: Value) extends Operation {

  override val aType =
    if (!left.isList && !right.isList)
      left.aType.multiplication(right.aType).getOrElse {
        val lType = left.aType
        val rType = right.aType
        throw new TypeCheckException(s"Cannot multiply a $rType with a $lType.")
      }
    else
      throw new UnsupportedOperationException("Cannot multiply lists.")
  
  override val isList = false
  
  override def getConcreteValue(ctx: EvaluationCtx): ConcreteValue = {
    val leftValue = left.getConcreteValue(ctx)
    val rightValue = right.getConcreteValue(ctx)
    
    leftValue.multiply(rightValue)
  }
}

case class Division(left: Value, right: Value) extends Operation {

  override val aType =
    if (!left.isList && !right.isList)
      left.aType.division(right.aType).getOrElse {
        val lType = left.aType
        val rType = right.aType
        throw new TypeCheckException(s"Cannot divide a $rType by a $lType.")
      }
    else
      throw new UnsupportedOperationException("Cannot divide lists.")
  
  override val isList = false
  
  override def getConcreteValue(ctx: EvaluationCtx): ConcreteValue = {
    val leftValue = left.getConcreteValue(ctx)
    val rightValue = right.getConcreteValue(ctx)
    
    leftValue.divide(rightValue)
  }
}

case class AbsoluteValue(value: Value) extends Operation {

  override val aType =
    if (!value.isList)
      value.aType.absoluteValue().getOrElse {
        val vType = value.aType
        throw new TypeCheckException(s"Cannot take the absolute value of a $vType.")
      }
    else
      throw new UnsupportedOperationException("Cannot take the absolute value of a list.")
  
  override val isList = false
  
  override def getConcreteValue(ctx: EvaluationCtx): ConcreteValue = {
    val cValue = value.getConcreteValue(ctx)
    
    cValue.abs()
  }
}