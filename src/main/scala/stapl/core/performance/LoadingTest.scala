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
import stapl.core.examples.EdocsPolicy

object EhealthAttributes {
  val subject = EhealthPolicy.subject
  val resource = EhealthPolicy.resource
  val action = EhealthPolicy.action
  val env = EhealthPolicy.environment
}
object EdocsAttributes {  
  val subject = EdocsPolicy.subject
  val resource = EdocsPolicy.resource
  val action = EdocsPolicy.action
  val env = EdocsPolicy.environment
}
object ArtificialAttributes {
  val subject = stapl.core.subject
  val resource = stapl.core.resource
  val action = stapl.core.action
  val env = stapl.core.environment
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
  override def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, 
      aType: AttributeType, multiValued: Boolean) = {
    // Note: we do not check the multiValued boolean, we just return single-valued attributes.
    if (name.startsWith("attribute")) {
      Some("rubbish")
    } else {
      None
    }
  }
}

/**
 * The tests used for evaluating the policy loading performance in the paper.
 */
object LoadingTest extends App {

  val policySizes = List(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200)

  val policyHome = args(0)
  val policyName = args(1)
  val nbWarmups = args(2).toInt
  val nbRuns = args(3).toInt

  if (policyName == "ehealth") {
    test("ehealth", policyHome + "/ehealth.stapl", "stapl.core.tests.performance.EhealthAttributes._", testEhealthPolicy, nbWarmups, nbRuns)
  } else if (policyName == "edocs") {
    test("edocs", policyHome + "/edocs.stapl", "stapl.core.tests.performance.EdocsAttributes._", testEdocsPolicy, nbWarmups, nbRuns)
  } else {
    val policySize = policyName.toInt
    if (!policySizes.contains(policySize)) {
      println("Invalid policy size given: " + policySize + " Valid policy sizes: " + policySizes)
    } else {
      // then test the artificial policies
      val parser = new PolicyParser
      test(f"$policySize%d", policyHome + "/large-policy-l1-p" + policySize + "-a20.stapl", "stapl.core.tests.performance.ArtificialAttributes._", testArtificialPolicy, nbWarmups, nbRuns)
    }
  }

  def testEhealthPolicy(policy: AbstractPolicy) = {
    import EhealthAttributes._

    val attributeFinder = new AttributeFinder

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
    if (result.decision != Deny) {
      throw new RuntimeException("The policy did not evaluate correctly!")
    }
  }

  def testEdocsPolicy(policy: AbstractPolicy) = {
    import EdocsAttributes._

    val attributeFinder = new AttributeFinder

    val pdp = new PDP(policy, attributeFinder)
    val result = pdp.evaluate("maarten", "create", "subtenantX",
        subject.role -> List("junior"),
        subject.department -> "another-department",
        subject.tenant -> "large-bank",
        subject.tenant_name -> List("large-bank"),
        subject.tenant_type -> List("tenant"),
        resource.type_ -> "subtenant",
        resource.confidential -> false,
        resource.owning_tenant -> "large-bank")
    if (result.decision != Deny) {
      throw new RuntimeException("The policy did not evaluate correctly!")
    }
  }

  def testArtificialPolicy(policy: AbstractPolicy) = {
    import ArtificialAttributes._

    val attributeFinder = new AttributeFinder
    attributeFinder += new ArtificialAttributeFinderModule

    val pdp = new PDP(policy, attributeFinder)
    val result = pdp.evaluate("maarten", "create", "subtenantX")
    if (result.decision != NotApplicable) {
      throw new RuntimeException("The policy did not evaluate correctly!")
    }
  }

  def read(path: String) = {
    val source = io.Source.fromFile(path)
    val policyString = source.mkString
    source.close
    policyString
  }

  def test(label: String, path: String, attribute_import: String, testPolicy: (AbstractPolicy) => Unit, nbWarmups: Int, nbRuns: Int) = {
    val timer = new Timer
    import timer.time
    val parser = new PolicyParser
    parser.addImport(attribute_import)
    import parser.parse

    val policyString = read(path)

    // TODO programatically disable log output on stdout
    
    // test the policy
    val policy = time { parse(policyString) }
    testPolicy(policy)
    
    // do the warmups
    for(i <- 1 until nbWarmups) {
      parse(policyString)
    }

    println(s"Initial loading time = ${timer.mean} ms")

    timer.reset
    for (i <- 1 until nbRuns) {
      time { parse(policyString) }
    }

    println(f"Loading - STAPL - $label - ${timer.mean}%2.2f ms ($nbWarmups warmups, $nbRuns runs)")
    //println(f"   (details: stdDev = ${timer.stdDev}%2.2f, confInterval = ${timer.confInt() * 100}%2.2f%%)")
    //println(s"Timings: ${timer.timings.reverse}")

  }
}