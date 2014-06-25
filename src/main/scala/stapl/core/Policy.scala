package stapl.core

import grizzled.slf4j.Logging
import stapl.core.pdp.EvaluationCtx

/*********************************************
 * The basic constructors
 */
abstract class AbstractPolicy(val id:String) {
  var parent: Option[PolicySet] = None
  
  /**
   * Each element in the policy tree should only return Obligations which
   * apply its decision.
   */
  def evaluate(ctx: EvaluationCtx): Result
  
  def isApplicable(ctx: EvaluationCtx): Boolean
  
  def allIds: List[String]
  
  /**
   * Returns the ordered list of all ids from the top of the policy tree
   * to this element of the policy tree, this element first and working to the top.
   */  
  def treePath: List[String] = parent match {
    case Some(parent) => id :: parent.treePath
    case None => List(id)
  }
  
  /**
   * Returns the fully qualified id of this element of the policy tree.
   * This id is the concatenation of all ids of the elements on the tree
   * path of this element, starting from the top and working down. 
   */
  def fqid: String = treePath.reverse.mkString(">") // TODO performance optimization: cache this stuff
}

class Policy(id: String)(val target: Expression=AlwaysTrue, val effect: Effect, 
    var condition: Expression=AlwaysTrue, val obligations: List[Obligation] = List.empty) 
	extends AbstractPolicy(id) with Logging {
  
  override def evaluate(ctx:EvaluationCtx): Result = {
    debug("FLOW: starting evaluation of Policy #" + fqid)
    if (!isApplicable(ctx)) {
      debug(s"FLOW: Policy #$fqid was NotApplicable because of target")
      NotApplicable
    } else {
      if (condition.evaluate(ctx)) {
    	val applicableObligations = obligations.filter(_.fulfillOn == effect)
        debug(s"FLOW: Policy #$fqid returned $effect with obligations $applicableObligations")
    	Result(effect, applicableObligations)
      } else {
    	debug(s"FLOW: Policy #$fqid was NotApplicable because of condition")
        NotApplicable
      }
    }
  }
  
  override def isApplicable(ctx: EvaluationCtx): Boolean = target.evaluate(ctx)
  
  override def allIds: List[String] = List(id)
  
  override def toString = s"Policy #$fqid"
}

class PolicySet(id: String)(val target: Expression, val pca: CombinationAlgorithm, 
    val subpolicies: List[AbstractPolicy], val obligations: List[Obligation] = List.empty) 
	extends AbstractPolicy(id) with Logging {
  
  // assign this PolicySet as parent to the children
  subpolicies.foreach(_.parent = Some(this))
  
  require(!subpolicies.isEmpty, "A PolicySet needs at least one SubPolicy")
  //require(uniqueIds, "All policies require a unique ID")
  
  private def uniqueIds(): Boolean = {
    val ids = allIds
    val distinctIds = ids.distinct
    distinctIds.size == ids.size
  }
  
  override def evaluate(ctx: EvaluationCtx): Result = {
    debug(s"FLOW: starting evaluation of PolicySet #$fqid")
    if (isApplicable(ctx)) {
      val result = pca.combine(subpolicies, ctx)
      // add applicable obligations of our own
      val applicableObligations = result.obligations ::: obligations.filter(_.fulfillOn == result.decision)
      val finalResult = Result(result.decision, applicableObligations)
      debug(s"FLOW: PolicySet #$fqid returned $finalResult")
      finalResult
    } else {
      debug(s"FLOW: PolicySet #$fqid was NotApplicable because of target")
      NotApplicable
    }
  }
  
  override def isApplicable(ctx: EvaluationCtx): Boolean = target.evaluate(ctx)
  
  override def allIds: List[String] = id :: subpolicies.flatMap(_.allIds)
  
  override def toString = {
    val subs = subpolicies.toString
    s"PolicySet #$id = [${subs.substring(5, subs.length-1)}]"
  }
}


/****************************************
 * The more natural DSL for policies and policy sets
 * 
 * Examples for policies: 
 * 	Policy("policy1") := when ("role" in subject.roles) deny iff (subject.allowed === false)
 *  Policy("policy2") := deny iff (subject.allowed === false)
 *  Policy("policy3") := when ("role" in subject.roles) deny
 *  Policy("policy4") := deny
 *  
 * Examples for policy sets:
 * 	TODO
 */
class OnlyId(private val id: String) {
  
  def :=(t: TargetEffectConditionAndObligations): Policy =
    new Policy(id)(t.target, t.effect, t.condition, List(t.obligations: _*))
    
  def :=(t: TargetPCASubpoliciesAndObligations): PolicySet =
    new PolicySet(id)(t.target, t.pca, t.subpolicies, t.obligations)
	
  def :=(t: TargetEffectAndCondition): Policy =
    new Policy(id)(t.target, t.effect, t.condition)
    
  def :=(targetAndEffect: TargetAndEffect): Policy = 
    new Policy(id)(targetAndEffect.target, targetAndEffect.effect)
    
  def :=(onlyTarget: OnlyTarget): TargetAndId =
    new TargetAndId(id, onlyTarget.target)
 
  def :=(effectKeyword: EffectKeyword): Policy = effectKeyword match {
    case `deny` => new Policy(id)(AlwaysTrue, Deny)
    case `permit` => new Policy(id)(AlwaysTrue, Permit)
  }
  
  def :=(t: TargetPCAAndSubpolicies): PolicySet =
    new PolicySet(id)(t.target, t.pca, List(t.subpolicies: _*))
    
}

class ObligationActionWithOn(val obligationAction: ObligationAction) {
  
  def on(effect: Effect): Obligation =
    new Obligation(obligationAction, effect)
}

class TargetEffectAndCondition(val target: Expression, val effect: Effect, val condition: Expression) {
  
  def performing(obligations: Obligation*): TargetEffectConditionAndObligations = 
    new TargetEffectConditionAndObligations(target, effect, condition, obligations: _*)
}

class TargetEffectConditionAndObligations(val target: Expression, 
    val effect: Effect, val condition: Expression, val obligations: Obligation*)

class TargetAndEffect(val target: Expression, val effect: Effect) {
  
  def iff(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(target, effect, condition)
}
class EffectKeyword // FIXME this cannot be the best way to do this...
case object deny extends EffectKeyword {
  /**
   * Needed if no target is given
   */
  def iff(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(AlwaysTrue, Deny, condition)
}
case object permit extends EffectKeyword {  
  /**
   * Needed if no target is given
   */
  def iff(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(AlwaysTrue, Permit, condition)
}
class TargetAndId(val id: String, val target: Expression) {
  
  def permit: Policy =
    new Policy(id)(target, Permit)
  
  def deny: Policy =
    new Policy(id)(target, Deny)
}

class TargetPCAAndSubpolicies(val target: Expression, val pca: CombinationAlgorithm, val subpolicies: AbstractPolicy*) {
  
  def performing(obligations: Obligation*): TargetPCASubpoliciesAndObligations = 
    new TargetPCASubpoliciesAndObligations(target, pca, List(subpolicies: _*), List(obligations: _*))
} 

class TargetPCASubpoliciesAndObligations(val target: Expression, val pca: CombinationAlgorithm, 
    val subpolicies: List[AbstractPolicy], val obligations: List[Obligation])

class TargetAndPCA(val target: Expression, val pca: CombinationAlgorithm) {
  
  def to(subpolicies: AbstractPolicy*): TargetPCAAndSubpolicies =
    new TargetPCAAndSubpolicies(target, pca, subpolicies: _*)
}

class OnlyTarget(val target: Expression) {
  
  def permit(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(target, Permit, condition)
  
  def deny(condition: Expression): TargetEffectAndCondition =
    new TargetEffectAndCondition(target, Deny, condition)
  
  def apply(pca: CombinationAlgorithm): TargetAndPCA =
    new TargetAndPCA(target, pca)
  
}
object when {
  def apply(target: Expression = AlwaysTrue): OnlyTarget =
    new OnlyTarget(target)
}
object apply {
  
  /**
   * If no target is given for a policy set 
   */
  def apply(pca: CombinationAlgorithm): TargetAndPCA =
    new TargetAndPCA(AlwaysTrue, pca)
  
  def PermitOverrides(subpolicies: OnlySubpolicies): TargetPCAAndSubpolicies = 
    new TargetPCAAndSubpolicies(AlwaysTrue,stapl.core.PermitOverrides, subpolicies.subpolicies: _*)
}
class OnlySubpolicies(val subpolicies: AbstractPolicy*)
object to {
  
  def apply(subpolicies: AbstractPolicy*): OnlySubpolicies =
    new OnlySubpolicies(subpolicies: _*)
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
    new OnlyId(id)
}
object PolicySet {
  def apply(id: String) =
    new OnlyId(id)
}