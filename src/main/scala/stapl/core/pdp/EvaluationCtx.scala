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