/*
 * Copyright 2015 Jasper Moeys, iMinds-DistriNet, KU Leuven
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
import scala.language.implicitConversions

abstract class Expression {
  def evaluate(ctx: EvaluationCtx): Boolean

  final def &(that: Expression): Expression = And(this, that)

  final def |(that: Expression): Expression = Or(this, that)

  final def unary_!(): Expression = Not(this)
}

object Expression {
  implicit def boolean2expression(value: Boolean): Expression = LiteralExpression(value)
}

case class ValueExpression(value: Value[Boolean]) extends Expression {
  override def evaluate(ctx: EvaluationCtx): Boolean = value.getConcreteValue(ctx)
}

case class LiteralExpression(value: Boolean) extends Expression {
  override def evaluate(ctx: EvaluationCtx): Boolean = value
}

case class And(expression1: Expression, expression2: Expression) extends Expression {
  override def evaluate(ctx: EvaluationCtx) = expression1.evaluate(ctx) && expression2.evaluate(ctx)
}
case class Or(expression1: Expression, expression2: Expression) extends Expression {
  override def evaluate(ctx: EvaluationCtx) = expression1.evaluate(ctx) || expression2.evaluate(ctx)
}
case class Not(expression: Expression) extends Expression {
  override def evaluate(ctx: EvaluationCtx) = !expression.evaluate(ctx)
}
