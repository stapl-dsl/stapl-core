package stapl.core

import stapl.core.Attribute

trait EvaluationCtx {
  
  def subjectId: String
  def resourceId: String
  def actionId: String
  protected[core] def findAttribute(attribute: Attribute): ConcreteValue
}

class BasicEvaluationCtx(request: RequestCtx, finder: AttributeFinder) extends EvaluationCtx {
  
  override val subjectId: String = request.subjectId
  
  override val resourceId: String = request.resourceId
  
  override val actionId: String = request.actionId
  
  final val cachedAttributes: scala.collection.mutable.Map[Attribute,ConcreteValue] = request.allAttributes
                                                           
  override def findAttribute(attribute: Attribute): ConcreteValue =
    cachedAttributes.getOrElse(attribute, finder.find(this, attribute)) // TODO add to cache
}