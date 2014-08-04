package stapl.core.tests.performance

import stapl.core.examples.EhealthPolicy
import stapl.core.examples.EdocsPolicy
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
import stapl.core.Permit
import stapl.core.AttributeContainer

object EvaluationTest extends App {

  val policyHome = args(0)
  val nbRuns = args(1).toInt
  val nbEvaluationsPerRun = args(2).toInt

  runEhealthTests(nbRuns, nbEvaluationsPerRun)
  runEdocsTests(nbRuns, nbEvaluationsPerRun)
  runArtificialTests(nbRuns, nbEvaluationsPerRun)
  
  

  def runEhealthTests(nbRuns: Int, nbEvaluationsPerRun: Int) = {
    println("================================================")
    println(f"Starting test E-health ($nbRuns%d runs, $nbEvaluationsPerRun%d evaluations per run)")
    println("================================================")

    import EhealthPolicy._

    val finder = new AttributeFinder
    finder += new ArtificialAttributeFinderModule
    val pdp = new PDP(naturalPolicy, finder)

    val timer = new Timer

    // then run the tests
    for (n <- 0 until nbRuns) {
      timer.time {
        for (n <- 0 until nbEvaluationsPerRun) {
          val result = pdp.evaluate("maarten", "view", "doc123",
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
            env.currentDateTime -> new LocalDateTime(2014, 6, 24, 14, 2, 1))
          if (result.decision != Permit) {
            throw new RuntimeException("The policy did not evaluate correctly!")
          }
        }
      }
    }

    println(f"Mean evaluation time of $nbEvaluationsPerRun evaluations: ${timer.mean}%1.6f ms (stdDev: ${timer.stdDev}%1.6f, confInt: ${timer.confInt() * 100}%1.2f%%)")
    println(f"=> Mean evaluation time per evaluation: ${timer.mean * 1000.0 / nbEvaluationsPerRun}%1.3f microseconds")
    //println(timer.timings)
    println
  }

  def runEdocsTests(nbRuns: Int, nbEvaluationsPerRun: Int) = {
    println("================================================")
    println(f"Starting test E-docs ($nbRuns%d runs, $nbEvaluationsPerRun%d evaluations per run)")
    println("================================================")

    import EdocsPolicy._

    val finder = new AttributeFinder
    finder += new ArtificialAttributeFinderModule
    val pdp = new PDP(policy, finder)

    val timer = new Timer

    // then run the tests
    for (n <- 0 until nbRuns) {
      timer.time {
        for (n <- 0 until nbEvaluationsPerRun) {
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
      }
    }

    println(f"Mean evaluation time of $nbEvaluationsPerRun evaluations: ${timer.mean}%1.6f ms (stdDev: ${timer.stdDev}%1.6f, confInt: ${timer.confInt() * 100}%1.2f%%)")
    println(f"=> Mean evaluation time per evaluation: ${timer.mean * 1000.0 / nbEvaluationsPerRun}%1.3f microseconds")
    //println(timer.timings)
    println
  }

  def runArtificialTests(nbRuns: Int, nbEvaluationsPerRun: Int) = {
    val parser = new PolicyParser
    parser.addImport("stapl.core.tests.performance.ArtificialAttributes._")
    val policySizes = List(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200)
    val policyFiles = policySizes.map(x => (x, policyHome + "/large-policy-l1-p" + x + "-a20.stapl"))
    for ((size, policyFile) <- policyFiles) {
      val p = parser.parseFile(policyFile)
      runArtificialTest(f"Large policy: $size%d rules", p, nbRuns, nbEvaluationsPerRun)
    }
  }

  def runArtificialTest(label: String, policy: AbstractPolicy, nbRuns: Int, nbEvaluationsPerRun: Int) = {
    println("================================================")
    println(f"Starting test $label ($nbRuns%d runs, $nbEvaluationsPerRun%d evaluations per run)")
    println("================================================")

    val finder = new AttributeFinder
    finder += new ArtificialAttributeFinderModule
    val pdp = new PDP(policy, finder)

    val timer = new Timer

    // then run the tests
    for (n <- 0 until nbRuns) {
      timer.time {
        for (n <- 0 until nbEvaluationsPerRun) {
          val result = pdp.evaluate("maarten", "view", "doc123")
          if (result.decision != NotApplicable) {
            throw new RuntimeException("The policy did not evaluate correctly!")
          }
        }
      }
    }

    println(f"Mean evaluation time of $nbEvaluationsPerRun evaluations: ${timer.mean}%1.6f ms (stdDev: ${timer.stdDev}%1.6f, confInt: ${timer.confInt() * 100}%1.2f%%)")
    println(f"=> Mean evaluation time per evaluation: ${timer.mean * 1000.0 / nbEvaluationsPerRun}%1.3f microseconds")
    //println(timer.timings)
    println
  }

}