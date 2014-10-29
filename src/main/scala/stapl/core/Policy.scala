/**
 *    Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */
package stapl.core

import grizzled.slf4j.Logging
import stapl.core.pdp.EvaluationCtx

/*********************************************
 * The basic constructors
 */
abstract class AbstractPolicy(val id:String) {
  var parent: Option[Policy] = None
  
  /**
   * Each element in the policy tree should only return Obligations which
   * apply its decision.
   */
  def evaluate(ctx: EvaluationCtx): Result
  
  def isApplicable(ctx: EvaluationCtx): Boolean
  
  //def allIds: List[String]
  
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


/**
 * Represents one rule.
 */
class Rule(id: String)(val effect: Effect, 
    val condition: Expression=AlwaysTrue, val obligationActions: List[ObligationAction] = List.empty) 
	extends AbstractPolicy(id) with Logging {
  
  override def evaluate(ctx:EvaluationCtx): Result = {
    debug(s"FLOW: starting evaluation of Policy #$fqid (evaluation id #${ctx.evaluationId})")
    if (!isApplicable(ctx)) {
      debug(s"FLOW: Policy #$fqid was NotApplicable because of target")
      NotApplicable
    } else {
      if (condition.evaluate(ctx)) {
        debug(s"FLOW: Policy #$fqid returned $effect with obligations $obligationActions")
    	Result(effect, obligationActions)
      } else {
    	debug(s"FLOW: Policy #$fqid was NotApplicable because of condition")
        NotApplicable
      }
    }
  }
  
  override def isApplicable(ctx: EvaluationCtx): Boolean = true
  
  //override def allIds: List[String] = List(id)
  
  override def toString = s"Policy #$fqid"
}


/**
 * Represents a policy of one or more rules and/or subpolicies.
 */
class Policy(id: String)(val target: Expression = AlwaysTrue, val pca: CombinationAlgorithm, 
    val subpolicies: List[AbstractPolicy], val obligations: List[Obligation] = List.empty) 
	extends AbstractPolicy(id) with Logging {
  
  // assign this PolicySet as parent to the children
  subpolicies.foreach(_.parent = Some(this))
  
  require(!subpolicies.isEmpty, "A PolicySet needs at least one SubPolicy")
  //require(uniqueIds, "All policies require a unique ID")
  
  /*private def uniqueIds(): Boolean = {
    val ids = allIds
    val distinctIds = ids.distinct
    distinctIds.size == ids.size
  }*/
  
  override def evaluate(ctx: EvaluationCtx): Result = {
    debug(s"FLOW: starting evaluation of PolicySet #$fqid")
    if (isApplicable(ctx)) {
      val result = pca.combine(subpolicies, ctx)
      // add applicable obligations of our own
      val applicableObligationActions = result.obligationActions ::: obligations.filter(_.fulfillOn == result.decision).map(_.action)
      val finalResult = Result(result.decision, applicableObligationActions)
      debug(s"FLOW: PolicySet #$fqid returned $finalResult")
      finalResult
    } else {
      debug(s"FLOW: PolicySet #$fqid was NotApplicable because of target")
      NotApplicable
    }
  }
  
  override def isApplicable(ctx: EvaluationCtx): Boolean = target.evaluate(ctx)
  
  //override def allIds: List[String] = id :: subpolicies.flatMap(_.allIds)
  
  override def toString = {
    val subs = subpolicies.toString
    s"PolicySet #$id = [${subs.substring(5, subs.length-1)}]"
  }
}

/**
 * Represents a reference to a policy that resides at a remote location and should be evaluated
 * at that remote location.
 * 
 * TODO Do remote policy references reference the id or the fqid?
 */
case class RemotePolicy(override val id: String) extends AbstractPolicy(id) with Logging {
  
  override def evaluate(ctx: EvaluationCtx): Result = {
    debug(s"FLOW: starting evaluation of Remote Policy #$fqid (evaluation id #${ctx.evaluationId})")
    val result = ctx.remoteEvaluator.findAndEvaluate(id, ctx)
    // TODO Filter obligations?
    debug(s"FLOW: Remote Policy #$fqid returned $result")
    result
  }
  
  /**
   * This method shouldn't be called during the evaluation of this policy, but an implementation is 
   * provided for the case where one would like to know whether a policy is applicable in a certain 
   * EvalutionCtx without evaluating it (???).
   */
  override def isApplicable(ctx: EvaluationCtx): Boolean = {
    warn("We are checking whether a remote policy is applicable without evaluating the policy. Are you sure this is what you want to do?")
    ctx.remoteEvaluator.findAndIsApplicable(id, ctx)
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
 *  
 * FIXME the "policy" in this line should not be possible:
 * 	Policy("view document") := when (action.id === "view" & resource.type_ === "document") permit
 *  ====== Why not? ======
 *  
 * TODO does a rule need a target?
 */
class OnlyIdRule(private val id: String) {
  
  def :=(t: EffectConditionAndObligationActions): Rule =
    new Rule(id)(t.effect, t.condition, List(t.obligationActions: _*))
	
  def :=(t: EffectAndCondition): Rule =
    new Rule(id)(t.effect, t.condition)
 
  def :=(effectKeyword: EffectKeyword): Rule = effectKeyword match {
    case `deny` => new Rule(id)(Deny)
    case `permit` => new Rule(id)(Permit)
  }
    
}

class OnlyIdPolicy(private val id: String) {
  
  def :=(t: TargetPCASubpoliciesAndObligations): Policy =
    new Policy(id)(t.target, t.pca, t.subpolicies, t.obligations)
    
  def :=(t: TargetPCAAndSubpolicies): Policy =
    new Policy(id)(t.target, t.pca, List(t.subpolicies: _*))
}

class ObligationActionWithOn(val obligationAction: ObligationAction) {
  
  def on(effect: Effect): Obligation =
    new Obligation(obligationAction, effect)
}

class EffectAndCondition(val effect: Effect, val condition: Expression) {
  
  def performing(obligationActions: ObligationAction*): EffectConditionAndObligationActions = 
    new EffectConditionAndObligationActions(effect, condition, obligationActions: _*)
}

class EffectConditionAndObligationActions( 
    val effect: Effect, val condition: Expression, val obligationActions: ObligationAction*)


class EffectKeyword // FIXME this cannot be the best way to do this...
case object deny extends EffectKeyword {
  /**
   * Needed if no target is given
   */
  def iff(condition: Expression): EffectAndCondition =
    new EffectAndCondition(Deny, condition)
}
case object permit extends EffectKeyword {  
  /**
   * Needed if no target is given
   */
  def iff(condition: Expression): EffectAndCondition =
    new EffectAndCondition(Permit, condition)
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
  
  def DenyOverrides(subpolicies: OnlySubpolicies): TargetPCAAndSubpolicies = 
    new TargetPCAAndSubpolicies(AlwaysTrue,stapl.core.DenyOverrides, subpolicies.subpolicies: _*)
  
  def FirstApplicable(subpolicies: OnlySubpolicies): TargetPCAAndSubpolicies = 
    new TargetPCAAndSubpolicies(AlwaysTrue,stapl.core.FirstApplicable, subpolicies.subpolicies: _*)
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
object Rule { // not really a companion object of Rule, but the start of the natural DSL for policies
  def apply(id: String) =
    new OnlyIdRule(id)
}
object Policy {
  def apply(id: String) =
    new OnlyIdPolicy(id)
}