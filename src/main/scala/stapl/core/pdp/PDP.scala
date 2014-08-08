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
import stapl.core.ConcreteValue
import stapl.core.Decision
import stapl.core.Result

/**
 * Class used for representing a policy decision point (PDP). A PDP provides
 * access decisions by evaluating a policy.
 */
class PDP(policy: AbstractPolicy, attributeFinder: AttributeFinder) {
  
  /**
   * Set up this new PDP with an empty attribute finder (which does not find
   * any attributes).
   */
  def this(policy:AbstractPolicy) = this(policy, new AttributeFinder)
  
  /**
   * Evaluate the policy of this PDP with given subject id, action id, resource id
   * and possibly extra attributes and return the result.
   * This will employ the attribute finder of this PDP.
   */
  def evaluate(subjectId: String, actionId: String, 
    resourceId: String, extraAttributes: (Attribute,ConcreteValue)*): Result =
      evaluate(new RequestCtx(subjectId, actionId, resourceId, extraAttributes: _*))
  
  /**
   * Evaluate the policy of this PDP with given request context and return the result.
   * This will employ the attribute finder of this PDP.
   */
  def evaluate(ctx: RequestCtx): Result = evaluate(new BasicEvaluationCtx(ctx, attributeFinder))
  
  /**
   * Evaluate the policy of this PDP with given evaluation context and return
   * the result. 
   * This allows you to specify another attribute finder than the one of this PDP.
   */
  def evaluate(ctx: EvaluationCtx): Result = policy.evaluate(ctx)
}