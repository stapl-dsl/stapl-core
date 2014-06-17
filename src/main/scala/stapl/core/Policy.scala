package stapl.core

abstract class AbstractPolicy(val id:String) {
  def evaluate(ctx: EvaluationCtx): Result
  
  def isApplicable(ctx: EvaluationCtx): Boolean
  
  def allIds: List[String]
}

class Policy(id: String)(target: Expression=AlwaysTrue, effect: Effect, condition: Expression=AlwaysTrue) extends AbstractPolicy(id) {
  override def evaluate(ctx:EvaluationCtx): Result =
    if (!isApplicable(ctx))
      NotApplicable
    else
      if (condition.evaluate(ctx))
        effect
      else
        NotApplicable
  
  override def isApplicable(ctx: EvaluationCtx): Boolean = target.evaluate(ctx)
  
  override def allIds: List[String] = List(id)
  
  override def toString = id
}

class PolicySet(id: String)(target: Expression, _subPolicies: AbstractPolicy*) extends AbstractPolicy(id) {
  algo: CombinationAlgorithm =>
  
  val subPolicies: List[AbstractPolicy] = List(_subPolicies:_*)
  
  require(!subPolicies.isEmpty, "A PolicySet needs at least one SubPolicy")
  require(uniqueIds, "All policies require a unique ID")
  
  private def uniqueIds(): Boolean = {
    val ids = allIds
    val distinctIds = ids.distinct
    distinctIds.size == ids.size
  }
  
  override def evaluate(ctx: EvaluationCtx) = 
    if (isApplicable(ctx))
      algo.combine(ctx)
    else
      NotApplicable
  
  override def isApplicable(ctx: EvaluationCtx): Boolean = target.evaluate(ctx)
  
  override def allIds: List[String] = id :: List(subPolicies:_*).flatMap(_.allIds)
  
  override def toString = {
    val subs = subPolicies.toString
    s"$id = [${subs.substring(5, subs.length-1)}]"
  }
}