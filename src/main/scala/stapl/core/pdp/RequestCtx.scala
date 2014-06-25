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