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
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.Map

/**
 * *************************
 * WRAPPERS
 *
 * We separate the keywords from the implementation so that these can be overridden
 * at runtime by passing them to the evaluation context.
 */
sealed trait CombinationAlgorithm {

  def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result =
    ctx.getCombinationAlgorithmImplementation(this).combine(policies, ctx)

  def combineAsync(policies: List[AbstractPolicy], ctx: EvaluationCtx): Future[Result] =
    ctx.getCombinationAlgorithmImplementation(this).combineAsync(policies, ctx)
}

case object PermitOverrides extends CombinationAlgorithm
case object DenyOverrides extends CombinationAlgorithm
case object FirstApplicable extends CombinationAlgorithm

/**
 * **********************
 * IMPLEMENTATIONS
 */
trait CombinationAlgorithmImplementation {

  def combine(policies: List[AbstractPolicy], ctx: EvaluationCtx): Result
  def combineAsync(policies: List[AbstractPolicy], ctx: EvaluationCtx): Future[Result]
}

trait CombinationAlgorithmImplementationBundle {
  def PermitOverrides: CombinationAlgorithmImplementation
  def DenyOverrides: CombinationAlgorithmImplementation
  def FirstApplicable: CombinationAlgorithmImplementation
}

/**
 * The bundle of simple implementations: sequential evaluation.
 */
object SimpleCombinationAlgorithmImplementationBundle extends CombinationAlgorithmImplementationBundle {

  object PermitOverrides extends CombinationAlgorithmImplementation {

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

    override def combineAsync(policies: List[AbstractPolicy], ctx: EvaluationCtx): Future[Result] = {
      for {
        results <- Future.sequence(policies map { _.evaluateAsync(ctx) })
      } yield {
        // check the results: 
        // If the list contains at least one Permit: return the first Permit
        // Else if the list contains at least one Deny: combine all Denies
        // Else return NotApplicable
        // TODO I guess you can do this without traversing the list twice
        val permits = results.filter(_.decision == Permit)
        val denies = results.filter(_.decision == Deny)
        if (!permits.isEmpty) {
          permits.head
        } else if (!denies.isEmpty) {
          var tmpResult = Result(Deny)
          denies foreach { x =>
            tmpResult = Result(Deny, tmpResult.obligationActions ::: x.obligationActions)
          }
          tmpResult
        } else {
          Result(NotApplicable)
        }
      }
      //      /**
      //       * This is part of the code that we need in order not to wait for every branch
      //       * to be evaluated. However, this needs synchronization => let's first try 
      //       * evaluation every branch
      //       */
      //      val promise = Promise[Result]()
      //      
      //      val intermediateResults = Map[AbstractPolicy,Option[Result]]()
      //
      //      policies foreach { x =>
      //        // store in the administration that we do not have any result for this policy yet
      //        intermediateResults(x) = None
      //        x.evaluateAsync(ctx) onSuccess {
      //        // we're permit overrides 
      //        // => if we receive a permit, check that all previous results are received and 
      //        // are NotApplicable or Deny. If so, we have a final decision. 
      //        // If we receive anything else, store it in the temporary results and check
      //        // if we received all results
      //          case result => result.decision match {
      //            case NotApplicable | Deny => 
      //              intermediateResults(x) = Some(result)
      //              // check completeness
      //              if(intermediateResults count { x => x._2.isEmpty } == 0) {
      //                
      //              } 
      //              for(result <- intermediateResults) {
      //                
      //              }
      //            case Permit =>
      //              
      //          }            
      //        }
      //      }
      //
      //      promise.future
    }
  }

  object DenyOverrides extends CombinationAlgorithmImplementation {

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

    override def combineAsync(policies: List[AbstractPolicy], ctx: EvaluationCtx): Future[Result] = {
      for {
        results <- Future.sequence(policies map { _.evaluateAsync(ctx) })
      } yield {
        // check the results: 
        // If the list contains at least one Deny: return the first Deny
        // Else if the list contains at least one Permit: combine all Permits
        // Else return NotApplicable
        // TODO I guess you can do this without traversing the list twice
        val denies = results.filter(_.decision == Deny)
        val permits = results.filter(_.decision == Permit)
        if (!denies.isEmpty) {
          denies.head
        } else if (!permits.isEmpty) {
          var tmpResult = Result(Deny)
          permits foreach { x =>
            tmpResult = Result(Permit, tmpResult.obligationActions ::: x.obligationActions)
          }
          tmpResult
        } else {
          Result(NotApplicable)
        }
      }
    }
  }

  object FirstApplicable extends CombinationAlgorithmImplementation {

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

    override def combineAsync(policies: List[AbstractPolicy], ctx: EvaluationCtx): Future[Result] = {
      for {
        results <- Future.sequence(policies map { _.evaluateAsync(ctx) })
      } yield {
        // check the results: return the first applicable result or NotApplicable if
        // there are no such results
        results.find(_.decision != NotApplicable) match {
          case Some(x) => x
          case None => NotApplicable
        } 
      }
    }
  }
}