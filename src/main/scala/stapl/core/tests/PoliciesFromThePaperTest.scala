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
package stapl.core.tests

import stapl.core.examples.PoliciesFromThePaper
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import stapl.core.pdp.PDP
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.RequestCtx
import org.junit.Assert._
import stapl.core.NotApplicable
import stapl.core.Deny
import stapl.core.Permit
import org.scalatest.junit.AssertionsForJUnit
import org.joda.time.LocalDateTime
import stapl.core.Result
import stapl.core.Obligation
import stapl.core.log

object PoliciesFromThePaperTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
class PoliciesFromThePaperTest extends AssertionsForJUnit {

  import PoliciesFromThePaper._
  
  // set up the PDP, use an empty attribute finder since we will provide all attributes in the request
  val pdp = new PDP(bigExample, new AttributeFinder)

  @Before def setup() {
    // nothing to do
  }

  @Test def testNurse() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("nurse"),
        subject.department -> "cardiology",
        subject.treated -> List("patient1","patient3"),
        resource.owner_id -> "patient3") === Result(Permit, List()))
  }
}