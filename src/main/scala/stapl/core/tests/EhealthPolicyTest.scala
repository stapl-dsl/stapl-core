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
import org.joda.time.LocalDateTime

object EhealthPolicyTest {
  
  @BeforeClass def setup() {
    // nothing to do
  }
}
class EhealthPolicyTest extends AssertionsForJUnit {

  import EhealthPolicy._
  // set up the PDP, use an empty attribute finder since we will provide all attributes in the request
  val pdp = new PDP(policy, new AttributeFinder)

  @Before def setup() {
    // nothing to do
  }

  @Test def testNotApplicableOtherAction() {
    assert(pdp.evaluate("maarten", "an-action", "doc123") === NotApplicable)
  }

  @Test def testDenyWithdrawnConsents() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel"),
        subject.triggered_breaking_glass -> false,
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) === Deny)
  }

  @Test def testDenyIncorrectMedicalPersonnel() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "role-not-allowed"),
        subject.triggered_breaking_glass -> false,
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) === Deny)
  }

  @Test def testPhysicianDepartment() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "department-not-allowed",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) === Deny)
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) === Permit)
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "elder_care",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) === Permit)
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "emergency",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) === Permit)
  }

  @Test def testPermitPhysicianEmergency() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3")) === Permit)
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"),
        resource.operator_triggered_emergency -> true) === Permit)
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3"),
        resource.operator_triggered_emergency -> false,
        resource.indicates_emergency -> true) === Permit)
  }

  @Test def testOverrideWithdrawnConsents() {
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "physician"),
        subject.triggered_breaking_glass -> true,
        subject.department -> "cardiology",
        resource.type_ -> "patientstatus",
        resource.owner_withdrawn_consents -> List("subject1","subject2","subject3","maarten")) === Permit)
  }
  
  @Test def testNurseOfElderCareDepartment() {
    // permit
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> true,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
        subject.responsible_patients -> List("patientX", "patientZ"),
        resource.owner_id -> "patientX",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        env.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1)) === Permit)
        
    // deny if not allowed to access the PMS
    assert(pdp.evaluate("maarten", "view", "doc123",
        subject.roles -> List("medical_personnel", "nurse"),
        subject.triggered_breaking_glass -> false,
        subject.department -> "elder_care",
        subject.allowed_to_access_pms -> false,
        subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
        subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
        subject.location -> "hospital",
        resource.owner_withdrawn_consents -> List("subject1"),
        resource.type_ -> "patientstatus",
        resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
        env.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1)) === Deny)
  }
}