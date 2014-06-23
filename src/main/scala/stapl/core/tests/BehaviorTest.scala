package stapl.core.tests

import stapl.core.examples.EhealthPolicy
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import stapl.core.PDP
import stapl.core.AttributeFinder
import stapl.core.RequestCtx
import org.junit.Assert._
import stapl.core.NotApplicable
import stapl.core.Deny
import stapl.core.Permit
import org.scalatest.junit.AssertionsForJUnit

object BehaviorTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
class BehaviorTest extends AssertionsForJUnit {

  import EhealthPolicy._
  // set up the PDP, use an empty attribute finder since we will provide all attributes in the request
  val pdp = new PDP(policy, new AttributeFinder)

  @Before def setup() {
    // nothing to do
  }

  @Test def testWrongTypeGiven1() {
    intercept[StringIndexOutOfBoundsException] {
      pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List(1, 2, 3), // FIXME This should give a type exception, no?
        subject.triggered_breaking_glass -> false,
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) 
    } 
  }

  @Test def testWrongTypeGiven2() {
    intercept[StringIndexOutOfBoundsException] {
	  pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List(1, 2, 3), 
        subject.triggered_breaking_glass -> "blabla", // FIXME This should give a type exception, no?
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) 
    }
  }

  @Test def testWrongTypeGiven3() {
    intercept[StringIndexOutOfBoundsException] {
	  pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> "role", // FIXME This should give a type exception, no?
        subject.triggered_breaking_glass -> false, 
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) 
    }
  }

}