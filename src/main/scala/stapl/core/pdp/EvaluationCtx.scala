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

import grizzled.slf4j.Logging
import stapl.core.ConcreteValue
import stapl.core.pdp.RequestCtx
import stapl.core.Attribute
import stapl.core.pdp.AttributeFinder

trait EvaluationCtx {
  
  def subjectId: String
  def resourceId: String
  def actionId: String
  protected[core] def findAttribute(attribute: Attribute): ConcreteValue
}

class BasicEvaluationCtx(request: RequestCtx, finder: AttributeFinder) extends EvaluationCtx with Logging {
  
  override val subjectId: String = request.subjectId
  
  override val resourceId: String = request.resourceId
  
  override val actionId: String = request.actionId
  
  final val cachedAttributes: scala.collection.mutable.Map[Attribute,ConcreteValue] = request.allAttributes
                                                           
  override def findAttribute(attribute: Attribute): ConcreteValue = {
    cachedAttributes.get(attribute) match {
      case Some(value) => {
        debug("FLOW: found value of " + attribute + " in cache: " + value)
        value
      }
      case None => { // Not in the cache
        val value: ConcreteValue = finder.find(this, attribute)
        cachedAttributes(attribute) = value // add to cache
        debug("FLOW: retrieved value of " + attribute + ": " + value + " and added to cache")
        value
      }
    }
  }
}