package stapl.core.tests.performance

import stapl.core.examples.EhealthPolicy
import stapl.core.pdp.PDP
import stapl.core.pdp.AttributeFinder
import org.joda.time.LocalDateTime
import stapl.core.Result
import stapl.core.Deny
import stapl.core.AbstractPolicy
import stapl.core.parser.PolicyParser
import stapl.core.pdp.AttributeFinderModule
import stapl.core.AttributeContainerType
import stapl.core.AttributeType
import stapl.core.ConcreteValue
import stapl.core.pdp.EvaluationCtx
import stapl.core.SUBJECT
import stapl.core.RESOURCE
import stapl.core.ENVIRONMENT
import stapl.core.ACTION
import stapl.core.SimpleAttribute
import stapl.core.String
import stapl.core.Decision
import stapl.core.NotApplicable

/**
 * The attribute finder with the hard coded attributes for the performance tests 
 */
class HardcodedAttributeFinderModule extends AttributeFinderModule {

  override def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType): Option[ConcreteValue] = {
    cType match {
      case SUBJECT => name match {
        case "roles" => Some(List("medical_personnel", "nurse"))
        case "triggered_breaking_glass" => Some(false)
        case "department" => Some("elder_care")
        case "allowed_to_access_pms" => Some(true)
        case "shift_start" => Some(new LocalDateTime(2014, 6, 24, 9, 0, 0))
        case "shift_stop" =>Some( new LocalDateTime(2014, 6, 24, 17, 0, 0))
        case "location" => Some("hospital")
        case "admitted_patients_in_nurse_unit" => Some(List("patientX", "patientY"))
        case "responsible_patients" => Some(List("patientY", "patientZ"))
        case _ => Some("value") // for the artificial policies
      }
      case RESOURCE => name match {
        case "owner_id" => Some("patientX")
        case "owner_withdrawn_consents" => Some(List("subject1"))
        case "type_" => Some("patientstatus")
        case "created" => Some(new LocalDateTime(2014, 6, 22, 14, 2, 1)) // three days ago
        case _ => Some("value") // for the artificial policies
      }
      case ENVIRONMENT => name match {
        case "currentDateTime" => new Some(new LocalDateTime(2014, 6, 24, 14, 2, 1))
        case _ => Some("value") // for the artificial policies
      }
      case ACTION => Some("value") // for the artificial policies
    }
  }
}

object EvaluationTest extends App {
  
  val policyHome = args(0)
  val nbRuns = args(1).toInt
  val nbEvaluationsPerRun = args(2).toInt

  import EhealthPolicy._
  
  // warmup
  //runTests("Warmup", naturalPolicy, Deny)

  // first test the realistic ehealth policy
  runTests("E-health", naturalPolicy, Deny, nbRuns, nbEvaluationsPerRun)
  //runTests("Java-like policy", javaLikePolicy, Deny, nbRuns, nbEvaluationsPerRun)
  
  // then test the artificial policies
  var a = 0
  for (a <- 1 until 200) {
    subject.set("attribute" + a, SimpleAttribute(String))
  }
  val parser = new PolicyParser  
  val policySizes = List(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200)
  val policyFiles = policySizes.map(x => (x, policyHome + "/large-policy-l1-p" + x + "-a20.stapl"))
  for((size, policyFile) <- policyFiles) {
    val p = parser.parseFile(policyFile)
    runTests(f"Large policy: $size%d rules", p, NotApplicable, nbRuns, nbEvaluationsPerRun)
  }
  
//  val p = parser.parseFile(policyHome + "/large-policy-l1-p5-a20.stapl")
//  runTests("Correctness test", p, NotApplicable, 1, 1)
  
  

  def runTests(label: String, policy: AbstractPolicy, expected: Decision, nbRuns: Int = 10000, nbEvaluationsPerRun: Int = 1000) = {
    println("================================================")
    println(f"Starting test $label ($nbRuns%d runs, $nbEvaluationsPerRun%d evaluations per run)")
    println("================================================")

    val finder = new AttributeFinder
    finder += new HardcodedAttributeFinderModule
    val pdp = new PDP(policy, finder)

    val timer = new Timer

    // then run the tests
    for (n <- 0 until nbRuns) {
      timer.time {
        for (n <- 0 until nbEvaluationsPerRun) {
          val result = pdp.evaluate("maarten", "view", "doc123")
          if (result.decision != expected) {
            throw new RuntimeException("The policy did not evaluate correctly!")
          }
        }
      }
    }

    println(f"Mean evaluation time of $nbEvaluationsPerRun evaluations: ${timer.mean}%1.6f ms (stdDev: ${timer.stdDev}%1.6f, confInt: ${timer.confInt() * 100}%1.2f%%)")
    println(f"=> Mean evaluation time per evaluation: ${timer.mean * 1000.0 / nbEvaluationsPerRun}%1.3f microseconds")
    println(timer.timings)
    println
  }

}