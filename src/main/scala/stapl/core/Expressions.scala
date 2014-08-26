/**
 *    Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */
package stapl.core

import java.util.Date
import stapl.core.pdp.EvaluationCtx

abstract class Expression {
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