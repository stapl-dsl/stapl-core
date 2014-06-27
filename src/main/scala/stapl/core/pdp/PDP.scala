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
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.BasicEvaluationCtx
import stapl.core.ConcreteValue
import stapl.core.pdp.EvaluationCtx
import stapl.core.pdp.RequestCtx
import stapl.core.Decision
import stapl.core.Result

class PDP(policy: AbstractPolicy, attributeFinder: AttributeFinder) {
  
  def evaluate(subjectId: String, actionId: String, 
    resourceId: String, extraAttributes: (Attribute,ConcreteValue)*): Result =
      evaluate(new RequestCtx(subjectId, actionId, resourceId, extraAttributes: _*))
  
  def evaluate(ctx: RequestCtx): Result = policy.evaluate(new BasicEvaluationCtx(ctx, attributeFinder))
  
  def evaluate(ctx: EvaluationCtx): Result = policy.evaluate(ctx)
}