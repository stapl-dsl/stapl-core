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

import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import org.junit.Assert._
import org.scalatest.junit.AssertionsForJUnit
import stapl.core._

object RemoteTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
/**
 * Some tests about the handling of remote policies.
 * There are no real remote policies involved in these tests.
 */
class RemoteTest extends AssertionsForJUnit {

  // a RemoteEvaluator with a stub module
  val evaluator = new RemoteEvaluator
  evaluator += new RemoteEvaluatorModule{
    override def findAndEvaluate(policyId: String, ctx: EvaluationCtx) = policyId match {
      case "always-permit-policy" => Some(Result(Permit))
      case "always-deny-policy" => Some(Result(Deny))
      case _ => None
    }
    
    override def findAndIsApplicable(policyId: String, ctx: EvaluationCtx) = ??? // shouldn't be called anyway
  }
  
  @Before def setup() {
    // nothing to do
  }

  @Test def testPermit() {
    val policy = RemotePolicy("always-permit-policy")
    
    val pdp = new PDP(policy, evaluator)
    assertEquals(Result(Permit), pdp.evaluate("subjectId", "actionId", "resourceId"))
  }
  
  @Test def testDeny() {
    val policy = RemotePolicy("always-deny-policy")
    
    val pdp = new PDP(policy, evaluator)
    assertEquals(Result(Deny), pdp.evaluate("subjectId", "actionId", "resourceId"))
  }
  
  @Test def testCannotFindPolicy() {
    intercept[RemotePolicyNotFoundException] {
	  val policy = RemotePolicy("unknown-policy")
	  
	  val pdp = new PDP(policy, evaluator)
	  pdp.evaluate("subjectId", "actionId", "resourceId")
    }
  }

}