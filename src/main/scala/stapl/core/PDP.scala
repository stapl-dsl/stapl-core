package stapl.core

class PDP(policy: AbstractPolicy, attributeFinder: AttributeFinder) {
  
  def evaluate(ctx: RequestCtx): Result = policy.evaluate(new BasicEvaluationCtx(ctx, attributeFinder))
  
  def evaluate(ctx: EvaluationCtx): Result = policy.evaluate(ctx)
}