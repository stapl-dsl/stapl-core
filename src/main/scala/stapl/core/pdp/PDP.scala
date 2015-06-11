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
package stapl.core.pdp

import stapl.core.AbstractPolicy
import stapl.core.Attribute
import stapl.core.Decision
import stapl.core.Result
import scala.collection.mutable.ListBuffer
import stapl.core.ObligationAction
import stapl.core.ConcreteObligationAction

/**
 * Class used for representing a policy decision point (PDP). A PDP provides
 * access decisions by evaluating a policy.
 */
class PDP(policy: AbstractPolicy,
  attributeFinder: AttributeFinder = new AttributeFinder,
  obligationService: ObligationService = new ObligationService,
  remoteEvaluator: RemoteEvaluator = new RemoteEvaluator) {

  private val timestampGenerator = new SimpleTimestampGenerator

  /**
   * Set up this new PDP with an empty attribute finder (which does not find
   * any attributes) and empty remote evaluator.
   */
  def this(policy: AbstractPolicy) = this(policy, new AttributeFinder, new ObligationService, new RemoteEvaluator)

  /**
   * Set up this new PDP with an empty attribute finder (which does not find
   * any attributes).
   */
  def this(policy: AbstractPolicy, remoteEvaluator: RemoteEvaluator) = this(policy, new AttributeFinder, new ObligationService, remoteEvaluator)

  /**
   * Set up this new PDP with an empty remote evaluator.
   */
  def this(policy: AbstractPolicy, attributeFinder: AttributeFinder) = this(policy, attributeFinder, new ObligationService, new RemoteEvaluator)

  /**
   * Evaluate the policy of this PDP with given subject id, action id, resource id
   * and possibly extra attributes and return the result.
   * This will employ the attribute finder of this PDP.
   */
  def evaluate(subjectId: String, actionId: String,
    resourceId: String, extraAttributes: (Attribute[_], Any)*): Result =
    evaluate(new RequestCtx(subjectId, actionId, resourceId, extraAttributes: _*))

  /**
   * Evaluate the policy of this PDP with given request context and generated incrementing
   * evaluation id and return the result. This will employ the attribute finder of this PDP.
   */
  def evaluate(ctx: RequestCtx): Result =
    evaluate(new BasicEvaluationCtx(timestampGenerator.getTimestamp, ctx, attributeFinder, remoteEvaluator))

  /**
   * Evaluate the policy of this PDP with given evaluation id and request context
   * and return the result. This will employ the attribute finder of this PDP.
   */
  def evaluate(evaluationId: String, ctx: RequestCtx): Result =
    evaluate(new BasicEvaluationCtx(evaluationId, ctx, attributeFinder, remoteEvaluator))

  /**
   * Evaluate the policy of this PDP with given evaluation context and return
   * the result.
   * This allows you to specify another attribute finder than the one of this PDP.
   *
   * The PDP will try to fulfill all obligations using the ObligationService and removes
   * the fulfilled obligations from the result.
   */
  def evaluate(ctx: EvaluationCtx): Result = {
    val result = policy.evaluate(ctx)
    // try to fulfill the obligations
    val remainingObligations = ListBuffer[ConcreteObligationAction]()
    for (obl <- result.obligationActions) {
      if (!obligationService.fulfill(obl)) {
        remainingObligations += obl
      }
    }
    // return the result with the remaining obligations
    new Result(result.decision, remainingObligations.toList, ctx.employedAttributes)
  }
}