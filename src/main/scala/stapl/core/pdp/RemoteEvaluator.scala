package stapl.core.pdp

import stapl.core.Result
import scala.annotation.tailrec
import stapl.core.RemotePolicyNotFoundException

/**
 * A class used for remotely evaluating a policy.
 * A RemoteEvaluator consists of (possibly) multiple RemoteEvaluatorModules, each of which are
 * capable of finding and evaluating certain policies.
 */
class RemoteEvaluator extends Modules[RemoteEvaluatorModule]{

  /**
   * Tries to find and evaluate the remote policy with id policyId in this EvaluationCtx.
   * The result of the first RemoteEvaluatorModule that returns Some result is returned. The 
   * others are ignored.
   * 
   * @return the result of evaluating the remote policy
   * @throws RemotePolicyNotFoundException if none of the RemoteEvaluatorModules can find the 
   *                                       remote policy
   */
  def findAndEvaluate(policyId: String, ctx: EvaluationCtx): Result = {
    @tailrec
    def findAndEvaluate(modules: List[RemoteEvaluatorModule]): Result = modules match {
      case module :: tail => module.findAndEvaluate(policyId, ctx) match {
        case Some(result) => result
        case None => findAndEvaluate(tail)
      }
      case Nil => throw new RemotePolicyNotFoundException(policyId)
    }
    findAndEvaluate(modules)
  }
  
  /**
   * Tries to find the remote policy with id policyId and test whether it is applicable in this 
   * EvaluationCtx. The result of the first RemoteEvaluatorModule that returns Some result is returned. 
   * The others are ignored.
   * 
   * @return true if the remote policy is applicable, false otherwise
   * @throws RemotePolicyNotFoundException if none of the RemoteEvaluatorModules can find the remote policy
   */
  def findAndIsApplicable(policyId: String, ctx: EvaluationCtx): Boolean = {
    @tailrec
    def findAndIsApplicable(modules: List[RemoteEvaluatorModule]): Boolean = modules match {
      case module :: tail => module.findAndIsApplicable(policyId, ctx) match {
        case Some(result) => result
        case None => findAndIsApplicable(tail)
      }
      case Nil => throw new RemotePolicyNotFoundException(policyId)
    }
    findAndIsApplicable(modules)
  }
  
}


/**
 * Trait for all RemoteEvaluatorModules passed to a RemoteEvaluator.
 */
trait RemoteEvaluatorModule {
  
  /**
   * Returns the result of evaluating the remote policy with id policyId in this
   * EvaluationCtx if he can find said policy. If the policy can't be found None
   * will be returned.
   */
  def findAndEvaluate(policyId: String, ctx: EvaluationCtx): Option[Result]
  
  /**
   * Returns the result of testing the applicability of the remote policy with id policyId 
   * in this EvaluationCtx if he can find said policy. If the policy can't be found None
   * will be returned.
   */
  def findAndIsApplicable(policyId: String, ctx: EvaluationCtx): Option[Boolean]
  
}