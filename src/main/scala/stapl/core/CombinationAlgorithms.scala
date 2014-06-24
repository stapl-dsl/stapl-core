package stapl.core

import scala.annotation.tailrec

trait CombinationAlgorithm {
  
  def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result
}

object PermitOverrides extends CombinationAlgorithm {
  
  override def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result = {
    @tailrec
    def combine(policyList: List[AbstractPolicy], tempResult: Result): Result = policyList match {
      case policy :: rest => policy.evaluate(ctx) match {
        case Permit => Permit
        case Deny => combine(rest, Deny)
        case NotApplicable => combine(rest, tempResult)
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
        case Deny => Deny
        case Permit => combine(rest, Permit)
        case NotApplicable => combine(rest, tempResult)
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
        case result@(Permit | Deny) => result
        case NotApplicable => combine(rest, NotApplicable)
      }
      case Nil => tempResult
    }
    
    combine(policies, NotApplicable)
  }
}