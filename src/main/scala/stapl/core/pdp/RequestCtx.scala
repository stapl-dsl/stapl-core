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
package stapl.core.pdp

import scala.collection.mutable.Map
import stapl.core.subject
import stapl.core.resource
import stapl.core.action
import stapl.core.Attribute
import stapl.core.ConcreteValue
import stapl.core.string2Value
import stapl.core.AttributeContainerType
import stapl.core.SUBJECT
import stapl.core.RESOURCE
import stapl.core.ACTION

/**
 * A class used for representing the context of a request.
 * For now, this is nothing more than the ids of the subject,
 * the action and the resource and any other attribute values given
 * with the request.
 *
 * Constructor: Initialize this new RequestCtx with given values. 
 * The ids of the subject, action and resource should always be 
 * given. Optionally, extra attributes can be provided (which should NOT
 * contain the ids of the subject, action and resource again).
 */
class RequestCtx(val subjectId: String, val actionId: String, 
    val resourceId: String, extraAttributes: (Attribute,ConcreteValue)*) {
  
  /*def this(subjectId: String, actionId: String, resourceId: String, extraAttributes: (Attribute,ConcreteValue)*) {
    this(subjectId, actionId, resourceId, extraAttributes.map{ case (attr, c) => ((attr.name, attr.cType), c) }: _*)
  }*/
  
  // TODO this is getting ugly(er)
  val allAttributes: Map[(String, AttributeContainerType), ConcreteValue] = Map(
      extraAttributes.map{ case (attr, c) => ((attr.name, attr.cType), c) }: _*)    
      
  // FIXME: these are not the same subject, resource and action as defined in BasicPolicy
  // For now, this works because the repeated definitions are the same, but we should'nt replicate
  // this definition. => idea: create a function to generate the subject, resoruce and action
  // and use this everwhere where you need subject/resource/action.id
  allAttributes += ("id", SUBJECT) -> subjectId
  allAttributes += ("id", RESOURCE) -> resourceId
  allAttributes += ("id", ACTION) -> actionId 
  
  override def toString(): String = f"${this.subjectId}--${this.actionId}->${this.resourceId} + ${this.allAttributes}" 
      
}