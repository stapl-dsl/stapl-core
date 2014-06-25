package stapl.core.pdp

import stapl.core.AbstractPolicy
import stapl.core.Attribute
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.BasicEvaluationCtx
import stapl.core.ConcreteValue
import stapl.core.pdp.EvaluationCtx
import stapl.core.pdp.RequestCtx
import stapl.core.Decision
import stapl.core.Result

class PDP(policy: AbstractPolicy, attributeFinder: AttributeFinder) {
  
  def evaluate(subjectId: String, actionId: String, 
    resourceId: String, extraAttributes: (Attribute,ConcreteValue)*): Result =
      evaluate(new RequestCtx(subjectId, actionId, resourceId, extraAttributes: _*))
  
  def evaluate(ctx: RequestCtx): Result = policy.evaluate(new BasicEvaluationCtx(ctx, attributeFinder))
  
  def evaluate(ctx: EvaluationCtx): Result = policy.evaluate(ctx)
}