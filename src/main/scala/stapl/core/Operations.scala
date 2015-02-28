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
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}

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
  
  override def getConcreteValue(ctx: EvaluationCtx) = {
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
  
  override def getConcreteValue(ctx: EvaluationCtx) = {
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
  
  override def getConcreteValue(ctx: EvaluationCtx) = {
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
  
  override def getConcreteValue(ctx: EvaluationCtx) = {
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
  
  override def getConcreteValue(ctx: EvaluationCtx) = {
    val cValue = value.getConcreteValue(ctx)
    
    cValue.abs()
  }
}