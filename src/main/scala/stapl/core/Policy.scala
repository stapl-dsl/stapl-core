/*
 * Copyright 2015 Jasper Moeys, iMinds-DistriNet, KU Leuven
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

/**
 * *******************************************
 * The basic constructors
 */
abstract class AbstractPolicy(val id: String) {
  var parent: Option[Policy] = None

  /**
   * Each element in the policy tree should only return Obligations which
   * apply its decision.
   */
  def evaluate(ctx: EvaluationCtx): Result

  // TODO remove this from AbstractPolicy, Policy and RemotePolicy (this is only to be used 
  // internally in a Policy)
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
  val condition: Expression = LiteralExpression(true), val obligationActions: List[ObligationAction] = List.empty)
  extends AbstractPolicy(id) with Logging {

  override def evaluate(ctx: EvaluationCtx): Result = {
    debug(s"FLOW: starting evaluation of Policy #$fqid (evaluation id #${ctx.evaluationId})")
    if (!isApplicable(ctx)) {
      debug(s"FLOW: Rule #$fqid was NotApplicable because of target")
      Result(NotApplicable)
    } else {
      if (condition.evaluate(ctx)) {
        debug(s"FLOW: Rule #$fqid returned $effect with obligations $obligationActions")
        Result(effect, obligationActions map { _.getConcrete(ctx) })
      } else {
        debug(s"FLOW: Rule #$fqid was NotApplicable because of condition")
        Result(NotApplicable)
      }
    }
  }

  /**
   * Rules always apply
   */
  override def isApplicable(ctx: EvaluationCtx): Boolean = true

  //override def allIds: List[String] = List(id)

  override def toString = s"Policy #$fqid"
}

/**
 * Represents a policy of one or more rules and/or subpolicies.
 */
class Policy(id: String)(val target: Expression = LiteralExpression(true), val pca: CombinationAlgorithm,
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
      val applicableObligationActions = result.obligationActions ::: obligations.filter(_.fulfillOn == result.decision).map(_.action.getConcrete(ctx))
      val finalResult = Result(result.decision, applicableObligationActions)
      debug(s"FLOW: PolicySet #$fqid returned $finalResult")
      finalResult
    } else {
      debug(s"FLOW: PolicySet #$fqid was NotApplicable because of target")
      Result(NotApplicable)
    }
  }

  override def isApplicable(ctx: EvaluationCtx): Boolean = target.evaluate(ctx)

  //override def allIds: List[String] = id :: subpolicies.flatMap(_.allIds)

  override def toString = {
    val subs = subpolicies.toString
    s"PolicySet #$id = [${subs.substring(5, subs.length - 1)}]"
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
