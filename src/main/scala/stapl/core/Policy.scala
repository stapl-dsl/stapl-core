package stapl.core

/*********************************************
 * The basic constructors
 */
abstract class AbstractPolicy(val id:String) {
  def evaluate(ctx: EvaluationCtx): Result
  
  def isApplicable(ctx: EvaluationCtx): Boolean
  
  def allIds: List[String]
}

class Policy(id: String)(val target: Expression=AlwaysTrue, val effect: Effect, var condition: Expression=AlwaysTrue) extends AbstractPolicy(id) {
  
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

class PolicySet(id: String)(val target: Expression, val pca: CombinationAlgorithm, _subPolicies: AbstractPolicy*) extends AbstractPolicy(id) {  
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
      pca.combine(subPolicies, ctx)
    else
      NotApplicable
  
  override def isApplicable(ctx: EvaluationCtx): Boolean = target.evaluate(ctx)
  
  override def allIds: List[String] = id :: List(subPolicies:_*).flatMap(_.allIds)
  
  override def toString = {
    val subs = subPolicies.toString
    s"$id = [${subs.substring(5, subs.length-1)}]"
  }
}


/****************************************
 * The more natural DSL
 * 
 * Example for policies: 
 * 	Policy("id")
 *  	deny "role" in subject.roles iff subject.allowed === false
 */
class IncompletePolicyWithId(private val id: String) {
  
  /**
   * If all elements are given
   */
  def :=(targetEffectAndCondition: TargetEffectAndCondition): Policy =
    new Policy(id)(targetEffectAndCondition.target, targetEffectAndCondition.effect, targetEffectAndCondition.condition)
  
  /**
   * If no condition is given
   */
  def :=(targetAndEffect: TargetAndEffect): Policy = 
    new Policy(id)(targetAndEffect.target, targetAndEffect.effect)
    
  def :=(onlyTarget: OnlyTarget): TargetAndId =
    new TargetAndId(id, onlyTarget.target)
    
}

class TargetEffectAndCondition(val target: Expression, val effect: Effect, val condition: Expression) 

class TargetAndEffect(val target: Expression, val effect: Effect) {
  
  def iff(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(target, effect, condition)
}
object deny {
  /**
   * Needed if no target is given
   */
  def iff(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(AlwaysTrue, Deny, condition)
}
object permit {  
  /**
   * Needed if no target is given
   */
  def iff(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(AlwaysTrue, Deny, condition)
}
class TargetAndId(val id: String, val target: Expression) {
  
  def permit: Policy =
    new Policy(id)(target, Permit)
  
  def deny: Policy =
    new Policy(id)(target, Deny)
}
class OnlyTarget(val target: Expression) {
  
  def permit(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(target, Permit, condition)
  
  def deny(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(target, Deny, condition)
  
}
object when {
  def apply(target: Expression = AlwaysTrue): OnlyTarget =
    new OnlyTarget(target)
}
object iff {
  /**
   * Just to add the keyword "iff"
   */
  def apply(condition: Expression): Expression =
    condition
}
object Policy { // not really a companion object of Policy, but the start of the natural DSL for policies
  def apply(id: String) =
    new IncompletePolicyWithId(id)
}