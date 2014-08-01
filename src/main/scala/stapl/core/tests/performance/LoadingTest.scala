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
import stapl.core.Decision
import stapl.core.examples.EhealthPolicy
import stapl.core.SimpleAttribute
import stapl.core.String
import stapl.core.NotApplicable
import stapl.core.pdp.AttributeFinderModule
import stapl.core.pdp.EvaluationCtx
import stapl.core.AttributeContainerType
import stapl.core.AttributeType
import stapl.core.ConcreteValue

object Attributes {
  val subject = EhealthPolicy.subject
  val resource = EhealthPolicy.resource
  val action = EhealthPolicy.action
  val env = EhealthPolicy.env
  var a = 0
  for (a <- 1 until 200) {
    subject.set("attribute" + a, SimpleAttribute(String))
  }  
}

/**
 * An attribute finder module for the artificial attribute1, attribute2, ... attributes.
 * Just returns "rubbish" for every attribute request, so that the artifical policies will
 * never apply.
 */
class ArtificialAttributeFinderModule extends AttributeFinderModule {
  
  /**
   * 
   */
  override def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType) = {
    if(name.startsWith("attribute")) {
      Some("rubbish")
    } else {
      None
    }
  }
}

object LoadingTest extends App {
  
  val policyHome = args(0)
  val nbRuns = args(1).toInt

  // test the policies from the case studies
  test("E-health", policyHome + "/ehealth.stapl", Deny, nbRuns)
  
  // then test the artificial policies
  import Attributes._
  val parser = new PolicyParser  
  val policySizes = List(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200)
  for(size <- policySizes) {
	  test(f"Large policy: $size%d rules", policyHome + "/large-policy-l1-p" + size + "-a20.stapl", NotApplicable, nbRuns)
  }
  
  
  

  def testPolicy(policy: AbstractPolicy, decision: Decision) = {
    import Attributes._

    val attributeFinder = new AttributeFinder
    attributeFinder += new ArtificialAttributeFinderModule
    
    val pdp = new PDP(policy, attributeFinder)
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
    if (result.decision != decision) {
      throw new RuntimeException("The policy did not evaluate correctly!")
    }
  }

  def read(path: String) = {
    val source = io.Source.fromFile(path)
    val policyString = source.mkString
    source.close
    policyString
  }

  def test(label: String, path: String, decision: Decision, nbRuns: Int = 1000) = {    
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

    testPolicy(policy, decision)

    println(s"Initial loading time = ${timer.mean} ms")

    timer.reset
    for (i <- 1 until nbRuns) {
      time { parse(policyString) }
    }

    println(f"Average loading time afterwards = ${timer.mean}%2.2f ms (stdDev = ${timer.stdDev}%2.2f, confInterval = ${timer.confInt() * 100}%2.2f%%)")
    //println(s"Timings: ${timer.timings.reverse}")

  }
}