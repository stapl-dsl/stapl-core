package stapl.core

import scala.collection.mutable.Map

class PDP(policy: AbstractPolicy, attributeFinder: AttributeFinder) {
  
  def evaluate(subjectId: String, actionId: String, 
    resourceId: String, extraAttributes: (Attribute,ConcreteValue)*): Result =
      evaluate(new RequestCtx(subjectId, actionId, resourceId, extraAttributes: _*))
  
  def evaluate(ctx: RequestCtx): Result = policy.evaluate(new BasicEvaluationCtx(ctx, attributeFinder))
  
  def evaluate(ctx: EvaluationCtx): Result = policy.evaluate(ctx)
}