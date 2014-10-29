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

import scala.annotation.tailrec
import stapl.core.pdp.EvaluationCtx

trait CombinationAlgorithm {

  def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result
}

object PermitOverrides extends CombinationAlgorithm {

  override def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result = {

    var tmpResult = Result(NotApplicable)

    for (policy <- policies) {
      val Result(decision, obligationActions, _) = policy.evaluate(ctx)
      // If a subpolicy returns Permit: return this result with its obligations,
      //	do not evaluate the rest for other obligations. 
      // TODO is this correct?
      // If all subpolicies return Deny: combine all their obligations and return 
      // 	them with Deny
      // If all subpolicies return NotApplicable: return NotApplicable without obligations
      // See XACML2 specs, Section 7.14
      decision match {
        case Permit => 
          // we only need one Permit and only retain those obligations => jump out of the loop
          return Result(decision, obligationActions) 
        case Deny =>
          // retain all obligations of previous Denies
          tmpResult = Result(Deny, tmpResult.obligationActions ::: obligationActions)
        case NotApplicable => // nothing to do
      }
    }
    // if we got here: return the tmpResult with Deny or NotApplicable
    tmpResult
  }
}

object DenyOverrides extends CombinationAlgorithm {

  override def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result = {

    var tmpResult = Result(NotApplicable)

    for (policy <- policies) {
      val Result(decision, obligationActions, _) = policy.evaluate(ctx)
      // If a subpolicy returns Deny: return this result with its obligations,
      //	do not evaluate the rest for other obligations. 
      // TODO is this correct?
      // If all subpolicies return Permit: combine all their obligations and return 
      // 	them with Permit
      // If all subpolicies return NotApplicable: return NotApplicable without obligations
      // See XACML2 specs, Section 7.14
      decision match {
        case Deny => 
          // we only need one Deny and only retain those obligations => jump out of the loop
          return Result(decision, obligationActions) 
        case Permit =>
          // retain all obligations of previous Permits
          tmpResult = Result(Permit, tmpResult.obligationActions ::: obligationActions)
        case NotApplicable => // nothing to do
      }
    }
    // if we got here: return the tmpResult with Permit or NotApplicable
    tmpResult
  }
}

object FirstApplicable extends CombinationAlgorithm {

  override def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result = {

    var tmpResult = Result(NotApplicable)

    for (policy <- policies) {
      val Result(decision, obligationActions, _) = policy.evaluate(ctx)
      decision match {
        case Permit | Deny => 
          // we only need one Deny or Permit and only retain those obligations => jump out of the loop
          return Result(decision, obligationActions) 
        case NotApplicable => // nothing to do
      }
    }
    // if we got here: return the tmpResult with Permit or NotApplicable
    tmpResult
  }
}