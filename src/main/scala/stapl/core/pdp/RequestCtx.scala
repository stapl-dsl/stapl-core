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

/**
 * A class used for ...
 *
 * Constructor: Initialize this new RequestCtx with given values. 
 * The ids of the subject, action and resource should always be 
 * given. Optionally, extra attributes can be provided (which should NOT
 * contain the ids of the subject, action and resource again).
 */
class RequestCtx(val subjectId: String, val actionId: String, 
    val resourceId: String, extraAttributes: (Attribute,ConcreteValue)*) {
  
  val allAttributes: Map[Attribute, ConcreteValue] = Map(
      extraAttributes: _*)    
  allAttributes += subject.id -> subjectId
  allAttributes += resource.id -> resourceId
  allAttributes += action.id -> actionId 
      
}