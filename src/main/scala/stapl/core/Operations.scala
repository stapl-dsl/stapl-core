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

import stapl.core.pdp.EvaluationCtx

case class BinaryOp[L, R, Out](name: String, l: Value[L], r: Value[R], op: (L, R) => Out) extends Value[Out] {
  def getConcreteValue(ctx: EvaluationCtx): Out = op(l.getConcreteValue(ctx), r.getConcreteValue(ctx))
}

case class UnaryOp[In, Out](name: String, in: Value[In], op: In => Out) extends Value[Out] {
  def getConcreteValue(ctx: EvaluationCtx): Out = op(in.getConcreteValue(ctx))
}