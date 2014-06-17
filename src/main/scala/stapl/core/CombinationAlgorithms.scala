package stapl.core

import scala.annotation.tailrec

trait CombinationAlgorithm {
  policySet: PolicySet =>
  
  def combine(ctx: EvaluationCtx): Result
}

trait PermitOverrides extends CombinationAlgorithm {
  policySet: PolicySet =>
  
  override def combine(ctx: EvaluationCtx): Result = {
    @tailrec
    def combine(policies: List[AbstractPolicy], tempResult: Result): Result = policies match {
      case policy :: rest => policy.evaluate(ctx) match {
        case Permit => Permit
        case Deny => combine(rest, Deny)
        case NotApplicable => combine(rest, tempResult)
      }
      case Nil => tempResult
    }
    
    combine(policySet.subPolicies, NotApplicable)
  }
}

trait DenyOverrides extends CombinationAlgorithm {
  policySet: PolicySet =>
  
  override def combine(ctx: EvaluationCtx): Result = {
    @tailrec
    def combine(policies: List[AbstractPolicy], tempResult: Result): Result = policies match {
      case policy :: rest => policy.evaluate(ctx) match {
        case Deny => Deny
        case Permit => combine(rest, Permit)
        case NotApplicable => combine(rest, tempResult)
      }
      case Nil => tempResult
    }
    
    combine(policySet.subPolicies, NotApplicable)
  }
}

trait FirstApplicable extends CombinationAlgorithm {
  policySet: PolicySet =>
  
  override def combine(ctx: EvaluationCtx): Result = {
    @tailrec
    def combine(policies: List[AbstractPolicy], tempResult: Result): Result = policies match {
      case policy :: rest => policy.evaluate(ctx) match {
        case result@(Permit | Deny) => result
        case NotApplicable => combine(rest, NotApplicable)
      }
      case Nil => tempResult
    }
    
    combine(policySet.subPolicies, NotApplicable)
  }
}