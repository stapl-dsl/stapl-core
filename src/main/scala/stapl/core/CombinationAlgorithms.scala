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
    @tailrec
    def combine(policyList: List[AbstractPolicy], tempResult: Result): Result = policyList match {
      case policy :: rest => policy.evaluate(ctx) match {
        case Result(decision, obligationActions) => decision match {
          // If a subpolicy returns Permit: return this result with its obligations,
          //	do not evaluate the rest for other obligations. 
          // TODO is this correct?
          // If all subpolicies return Deny: combine all their obligations and return 
          // 	them with Deny
          // If all subpolicies return NotApplicable: return NotApplicable without obligations
          // See XACML2 specs, Section 7.14
          case Permit => Result(decision, obligationActions) 				 
          case Deny => combine(rest, Result(Deny, tempResult.obligationActions ::: obligationActions)) 
          case NotApplicable => combine(rest, tempResult) 			
        } 
      }
      case Nil => tempResult
    }
    
    combine(policies, NotApplicable)
  }
}

object DenyOverrides extends CombinationAlgorithm {
  
  override def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result = {
    @tailrec
    def combine(policyList: List[AbstractPolicy], tempResult: Result): Result = policyList match {
      case policy :: rest => policy.evaluate(ctx) match {
        case Result(decision, obligationActions) => decision match {
          // If a subpolicy returns Deny: return this result with its obligations,
          //	do not evaluate the rest for other obligations. 
          // TODO is this correct?
          // If all subpolicies return Permit: combine all their obligations and return 
          // 	them with Permit
          // If all subpolicies return NotApplicable: return NotApplicable without obligations
          // See XACML2 specs, Section 7.14
	        case Deny => Result(decision, obligationActions)
	        case Permit => combine(rest, Result(Permit, tempResult.obligationActions ::: obligationActions))
	        case NotApplicable => combine(rest, tempResult)
        }
      }
      case Nil => tempResult
    }
    
    combine(policies, NotApplicable)
  }
}

object FirstApplicable extends CombinationAlgorithm {
  
  override def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result = {
    @tailrec
    def combine(policyList: List[AbstractPolicy], tempResult: Result): Result = policyList match {
      case policy :: rest => policy.evaluate(ctx) match {
        case Result(decision, obligations) => decision match {
          // Pass the decision and obligations of the first Permit or Deny
          case Permit => Result(decision, obligations)
          case Deny => Result(decision, obligations)
          case NotApplicable => combine(rest, NotApplicable)
        }
      }
      case Nil => tempResult
    }
    
    combine(policies, NotApplicable)
  }
}