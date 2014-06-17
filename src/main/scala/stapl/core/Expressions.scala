package stapl.core

import java.util.Date

sealed abstract class Expression {
  def evaluate(implicit ctx: EvaluationCtx): Boolean
  
  final def &(that: Expression): Expression = And(this, that)
  
  final def |(that: Expression): Expression = Or(this, that)
  
  final def unary_!(): Expression = Not(this)
}

case object AlwaysTrue extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = true
}
case object AlwaysFalse extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = false
}
case class GreaterThanValue(value1: Value, value2: Value) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = {
    val ConcreteValue(c1) = value1
    val ConcreteValue(c2) = value2
    c1.reprGreaterThan(c2)
  }
}
case class BoolExpression(attribute: SimpleAttribute) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = {
    val Representation(bool) = attribute
    bool.asInstanceOf[Boolean]
  }
}

case class EqualsValue(value1: Value, value2 : Value) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = {
    val ConcreteValue(c1) = value1
    val ConcreteValue(c2) = value2
    c1.equalRepr(c2)
  }
}
case class ValueIn(value: Value, list: Value) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx): Boolean = {
    val ConcreteValue(c) = value
    val ConcreteValue(l) = list
    l.reprContains(c)
  }
}
case class And(expression1: Expression, expression2: Expression) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx) = expression1.evaluate && expression2.evaluate
}
case class Or(expression1: Expression, expression2: Expression) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx) = expression1.evaluate || expression2.evaluate
}
case class Not(expression: Expression) extends Expression {
  override def evaluate(implicit ctx: EvaluationCtx) = !expression.evaluate
}