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
        case Result(decision, obligations) => decision match {
          // If a subpolicy returns Permit: return this result with its obligations,
          //	do not evaluate the rest for other obligations. 
          // TODO is this correct?
          // If all subpolicies return Deny: combine all their obligations and return 
          // 	them with Deny
          // If all subpolicies return NotApplicable: return NotApplicable without obligations
          // See XACML2 specs, Section 7.14
          case Permit => Result(decision, obligations) 				 
          case Deny => combine(rest, Result(Deny, tempResult.obligations ::: obligations)) 
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
        case Result(decision, obligations) => decision match {
          // If a subpolicy returns Deny: return this result with its obligations,
          //	do not evaluate the rest for other obligations. 
          // TODO is this correct?
          // If all subpolicies return Permit: combine all their obligations and return 
          // 	them with Permit
          // If all subpolicies return NotApplicable: return NotApplicable without obligations
          // See XACML2 specs, Section 7.14
	        case Deny => Result(decision, obligations)
	        case Permit => combine(rest, Result(Permit, tempResult.obligations ::: obligations))
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