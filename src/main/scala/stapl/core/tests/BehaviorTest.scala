package stapl.core.tests

import stapl.core.examples.EhealthPolicy
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import stapl.core.pdp.PDP
import stapl.core.pdp.AttributeFinder
import stapl.core.pdp.RequestCtx
import org.junit.Assert._
import stapl.core._
import org.scalatest.junit.AssertionsForJUnit

object BehaviorTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
class BehaviorTest extends AssertionsForJUnit {

  import EhealthPolicy._
  // set up the PDP, use an empty attribute finder since we will provide all attributes in the request
  val pdp = new PDP(naturalPolicy, new AttributeFinder)

  @Before def setup() {
    // nothing to do
  }
  
  @Test def testUndefinedAttribute() {
    val subject = stapl.core.subject // FIXME do we work on the single subject object here? we need a local copy of some sort
    val resource = stapl.core.resource
    val action = stapl.core.action
    val env = stapl.core.environment

    subject.roles = ListAttribute(String)

    intercept[AttributeDoesNotExistException] {
    	Policy("p") := permit iff (subject.nonexistingAttribute === "a-value")
    }
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