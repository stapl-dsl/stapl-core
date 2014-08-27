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

import stapl.core.examples.EdocsPolicy
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

object EdocsPolicyTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
/**
 * Correctness tests of the e-docs policy.
 */
class EdocsPolicyTest extends AssertionsForJUnit {

  import EdocsPolicy._
  // set up the PDP, use an empty attribute finder since we will provide all attributes in the request
  //val pdp = new PDP(javaLikePolicy, new AttributeFinder)
  val pdp = new PDP(policy, new AttributeFinder)

  @Before def setup() {
    // nothing to do
  }

  @Test def testDenyHelpdeskNotAssigned() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.role -> List("helpdesk"),
        subject.tenant_name -> List("provider"),
        subject.tenant_type -> List("provider"),
        subject.assigned_tenants -> List("tenant1","tenant3"),
        resource.type_ -> "document",
        resource.owning_tenant -> "tenant4",
        resource.confidential -> false) === Result(Deny, List()))
  }

  @Test def testPermitHelpdeskAssigned() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.role -> List("helpdesk"),
        subject.tenant_name -> List("provider"),
        subject.tenant_type -> List("provider"),
        subject.assigned_tenants -> List("tenant1","tenant3"),
        resource.type_ -> "document",
        resource.owning_tenant -> "tenant3",
        resource.confidential -> false) === Result(Permit, List()))
  }

  @Test def testPermitLargeBankCreateSubtenant() {
    assert(pdp.evaluate("maarten", "create", "subtenantX",
        subject.role -> List("senior"),
        subject.department -> "IT",
        subject.tenant -> "large-bank",
        subject.tenant_name -> List("large-bank"),
        subject.tenant_type -> List("tenant"),
        resource.type_ -> "subtenant",
        resource.confidential -> false,
        resource.owning_tenant -> "large-bank") === Result(Permit, List()))
  }

  @Test def testDenyLargeBankCreateSubtenantWrongDepartment() {
    assert(pdp.evaluate("maarten", "create", "subtenantX",
        subject.role -> List("senior"),
        subject.department -> "another-department",
        subject.tenant -> "large-bank",
        subject.tenant_name -> List("large-bank"),
        subject.tenant_type -> List("tenant"),
        resource.type_ -> "subtenant",
        resource.confidential -> false,
        resource.owning_tenant -> "large-bank") === Result(Deny, List()))
  }

  @Test def testDenyLargeBankCreateSubtenantWrongRole() {
    assert(pdp.evaluate("maarten", "create", "subtenantX",
        subject.role -> List("junior"),
        subject.department -> "another-department",
        subject.tenant -> "large-bank",
        subject.tenant_name -> List("large-bank"),
        subject.tenant_type -> List("tenant"),
        resource.type_ -> "subtenant",
        resource.confidential -> false,
        resource.owning_tenant -> "large-bank") === Result(Deny, List()))
  }
}