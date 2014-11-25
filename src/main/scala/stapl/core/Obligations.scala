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

/**
 * An obligation consists of an action that should be fulfilled and the 
 * effect on which the action should be fulfilled.
 */
case class Obligation(val action: ObligationAction, val fulfillOn: Effect)

/**
 * Traits for representing obligations: 
 * 
 * - ObligationAction: the obligation actions that can be specified in policies,
 * 		but can still contain attribute references that should be concretized
 *   	using the evaluation context.
 * 
 * - ConcreteObligationAction: the concretized obligation actions
 * 
 * - SimpleObligationAction: a simple trait for obligation actions that are can be specified
 * 		in policies, but do not need special logic to be concretized (e.g., they do not
 *   	contain attribute references, only literal values) 
 */
trait ObligationAction {
  
  def getConcrete(implicit ctx: EvaluationCtx): ConcreteObligationAction
}
trait ConcreteObligationAction
trait SimpleObligationAction extends ObligationAction with ConcreteObligationAction {
  
  override def getConcrete(implicit ctx: EvaluationCtx) = this
}

/**
 * Logging
 */
case class LogObligationAction(val msg: Value) extends ObligationAction {
  
  def getConcrete(implicit ctx: EvaluationCtx) = ConcreteLogObligationAction(msg.getConcreteValue(ctx).representation.toString)
}
case class ConcreteLogObligationAction(val msg: String) extends ConcreteObligationAction
object log {
  def apply(msg: Value) = new LogObligationAction(msg)
}

/**
 * Mailing
 */
case class MailObligationAction(val to: String, val msg: String) extends SimpleObligationAction
object mail {
  def apply(to: String, msg: String) = new MailObligationAction(to, msg)
}

/**
 * Updating attribute values
 */
case class UpdateAttributeObligationAction(val attribute: Attribute, val value: Value) extends ObligationAction {
  
  def getConcrete(implicit ctx: EvaluationCtx) = {
    val entityId = attribute.cType match {
      case SUBJECT => ctx.subjectId
      case RESOURCE => ctx.resourceId
      case _ => throw new IllegalArgumentException(s"You can only update SUBJECT and RESOURCE attributes. Given attribute: $attribute")
    }
    ConcreteUpdateAttributeObligationAction(entityId, attribute, value.getConcreteValue(ctx))
  }
}
case class ConcreteUpdateAttributeObligationAction(val entityId: String, val attribute: Attribute, val value: ConcreteValue) extends ConcreteObligationAction
object update {
  def apply(attribute: Attribute, value: Value) =
    new UpdateAttributeObligationAction(attribute, value)
}

/**
 * Appending to attribute values
 */
case class AppendAttributeObligationAction(val attribute: Attribute, val value: Value) extends ObligationAction {
  
  def getConcrete(implicit ctx: EvaluationCtx) = {
    val entityId = attribute.cType match {
      case SUBJECT => ctx.subjectId
      case RESOURCE => ctx.resourceId
      case _ => throw new IllegalArgumentException(s"You can only append to SUBJECT and RESOURCE attributes. Given attribute: $attribute")
    }
    ConcreteAppendAttributeObligationAction(entityId, attribute, value.getConcreteValue(ctx))
  }
}
case class ConcreteAppendAttributeObligationAction(val entityId: String, val attribute: Attribute, val value: ConcreteValue) extends ConcreteObligationAction
object append {
  def apply(attribute: Attribute, value: Value) =
    new AppendAttributeObligationAction(attribute, value)
}