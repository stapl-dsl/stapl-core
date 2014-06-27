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

import AttributeType._
import stapl.core.pdp.EvaluationCtx

sealed abstract class Attribute(cType: AttributeContainerType, name: String, aType: AttributeType) extends Value {
  
  override def getConcreteValue(ctx: EvaluationCtx) = 
    ctx.findAttribute(this)
}



case class ListAttribute(cType: AttributeContainerType, name: String, aType: AttributeType) 
  extends Attribute(cType, name, aType) {
  
  override val isList = true
}

object ListAttribute {
  import AttributeConstruction.UninitializedAttribute
  def apply(sType: AttributeType): UninitializedAttribute = 
    (None, new ListAttribute(_: AttributeContainerType, _: String, sType))
  
  def apply(name: String, sType: AttributeType): UninitializedAttribute = 
    (Option(name), new ListAttribute(_: AttributeContainerType, _: String, sType))
}



case class SimpleAttribute(cType: AttributeContainerType, name: String, aType: AttributeType) 
  extends Attribute(cType, name, aType) {
  
  override val isList = false
  
}

object SimpleAttribute {
  import AttributeConstruction.UninitializedAttribute
  def apply(sType: AttributeType): UninitializedAttribute = 
    (None, new SimpleAttribute(_: AttributeContainerType, _: String, sType))
  
  def apply(name: String, sType: AttributeType): UninitializedAttribute = 
    (Option(name), new SimpleAttribute(_: AttributeContainerType, _: String, sType))
}



object AttributeConstruction {
  private type AttributeConstructor = (AttributeContainerType, String) => Attribute
  type UninitializedAttribute = (Option[String], AttributeConstructor)
}
