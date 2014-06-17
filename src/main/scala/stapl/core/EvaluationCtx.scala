package stapl.core

trait EvaluationCtx {
  
  def subjectID: String
  def resourceID: String
  def actionID: String
  protected[core] def findAttribute(attribute: Attribute): ConcreteValue
}

class BasicEvaluationCtx(request: RequestCtx, finder: AttributeFinder) extends EvaluationCtx {
  
  override val subjectID: String = request.subjectID
  
  override val resourceID: String = request.resourceID
  
  override val actionID: String = request.actionID
  
  override def findAttribute(attribute: Attribute): ConcreteValue =
    request.attributes.getOrElse(attribute, finder.find(this, attribute))
}