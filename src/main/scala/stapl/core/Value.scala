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
import scala.language.implicitConversions

/**
 * The base trait for all elements of expressions that encapsulate concrete values. 
 * 
 * @tparam T The type of the concrete value this Value represents.
 */
trait Value[T] {
  def getConcreteValue(ctx: EvaluationCtx): T
}

object Value {
  /**
   * Wraps a concrete value of type T in a Value[T].
   */
  def apply[T](something: T) = new Value[T] { 
    def getConcreteValue(ctx: EvaluationCtx): T = something
    override def toString = something.toString
  }
  
  implicit def any2Value[T](t: T): Value[T] = Value(t)
}