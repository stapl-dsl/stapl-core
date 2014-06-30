package stapl.core.tests.performance

import stapl.core.AbstractPolicy
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.Settings
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.reporters.Reporter
import java.io.File
import stapl.core.pdp.PDP
import stapl.core.pdp.AttributeFinder
import org.joda.time.LocalDateTime
import stapl.core.Deny
import grizzled.slf4j.Logger
import stapl.core.parser.PolicyParser

object Attributes {
  import stapl.core._
  val subject = stapl.core.subject // FIXME do we work on the single subject object here? we need a local copy of some sort
  val resource = stapl.core.resource
  val action = stapl.core.action
  val env = stapl.core.environment
  env.currentDateTime = SimpleAttribute(DateTime)
  resource.type_ = SimpleAttribute(String)
  resource.owner_withdrawn_consents = ListAttribute(String)
  resource.operator_triggered_emergency = SimpleAttribute(Bool)
  resource.indicates_emergency = SimpleAttribute(Bool)
  resource.owner_id = SimpleAttribute("owner:id", String)
  resource.owner_responsible_physicians = ListAttribute("owner:responsible_physicians", String)
  resource.owner_discharged = SimpleAttribute("owner:discharged", Bool)
  resource.owner_discharged_dateTime = SimpleAttribute("owner:discharged_dateTime", DateTime)
  resource.patient_status = SimpleAttribute(String)
  resource.created = SimpleAttribute(DateTime)
  subject.roles = ListAttribute(String)
  subject.triggered_breaking_glass = SimpleAttribute(Bool)
  subject.department = SimpleAttribute(String)
  subject.current_patient_in_consultation = SimpleAttribute(String)
  subject.treated_in_last_six_months = ListAttribute(String)
  subject.primary_patients = ListAttribute(String)
  subject.is_head_physician = SimpleAttribute(Bool)
  subject.treated = ListAttribute(String)
  subject.treated_by_team = ListAttribute(String)
  subject.admitted_patients_in_care_unit = ListAttribute(String)
  subject.shift_start = SimpleAttribute(DateTime)
  subject.shift_stop = SimpleAttribute(DateTime)
  subject.location = SimpleAttribute(String)
  subject.admitted_patients_in_nurse_unit = ListAttribute(String)
  subject.allowed_to_access_pms = SimpleAttribute(Bool)
  subject.responsible_patients = ListAttribute(String)
}

object LoadingTest extends App {

  def testPolicy(policy: AbstractPolicy) = {
    import Attributes._

    val pdp = new PDP(policy, new AttributeFinder)
    val result = pdp.evaluate("maarten", "view", "doc123",
      subject.roles -> List("medical_personnel", "nurse"),
      subject.triggered_breaking_glass -> false,
      subject.department -> "elder_care",
      subject.allowed_to_access_pms -> true,
      subject.shift_start -> new LocalDateTime(2014, 6, 24, 9, 0, 0),
      subject.shift_stop -> new LocalDateTime(2014, 6, 24, 17, 0, 0),
      subject.location -> "hospital",
      subject.admitted_patients_in_nurse_unit -> List("patientX", "patientY"),
      subject.responsible_patients -> List("patientY", "patientZ"),
      resource.owner_id -> "patientX",
      resource.owner_withdrawn_consents -> List("subject1"),
      resource.type_ -> "patientstatus",
      resource.created -> new LocalDateTime(2014, 6, 22, 14, 2, 1), // three days ago
      env.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
    if (result.decision != Deny) {
      throw new RuntimeException("The policy did not evaluate correctly!")
    }
  }

  def read(path: String) = {
    val source = io.Source.fromFile(path)
    val policyString = source.mkString
    source.close
    policyString
  }

  def test(label: String, path: String, nbRuns: Int = 1000) = {    
    println("================================================")
    println(f"Starting test $label ($nbRuns%d runs)")
    println("================================================")
    val timer = new Timer
    import timer.time
    val parser = new PolicyParser
    import parser.parse

    val policyString = read(path)

    // TODO programatically disable log output on stdout 
    val policy = time { parse(policyString) }

    testPolicy(policy)

    println(s"Initial loading time = ${timer.mean} ms")

    timer.reset
    for (i <- 1 until nbRuns) {
      time { parse(policyString) }
    }

    println(f"Average loading time afterwards = ${timer.mean}%2.2f ms (stdDev = ${timer.stdDev}%2.2f, confInterval = ${timer.confInt() * 100}%2.2f%%)")
    //println(s"Timings: ${timer.timings.reverse}")

  }

  test("Natural policy", "/home/maartend/PhD/code/workspace-scala/stapl-core/resources/policies/ehealth-natural.stapl")
  test("Java-style policy", "/home/maartend/PhD/code/workspace-scala/stapl-core/resources/policies/ehealth.stapl")
}