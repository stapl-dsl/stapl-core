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

/**
 * The tests used for evaluating the policy evaluation performance in the paper.
 */
object EvaluationTest extends App {

  val policyHome = args(0)
  val test = args(1)
  val nbWarmups = args(2).toInt
  val nbRuns = args(3).toInt
  val nbEvaluationsPerRun = args(4).toInt

  if(test == "ehealth") {
	  runEhealthTests(nbWarmups, nbRuns, nbEvaluationsPerRun)
  } else if(test == "edocs") {
	  runEdocsTests(nbWarmups, nbRuns, nbEvaluationsPerRun)
  } else {
	val size = test.toInt
    runArtificialTests(size, nbWarmups, nbRuns, nbEvaluationsPerRun)
  }
  
  

  def runEhealthTests(nbWarmups: Int, nbRuns: Int, nbEvaluationsPerRun: Int) = {
    import EhealthPolicy._

    val finder = new AttributeFinder
    finder += new ArtificialAttributeFinderModule
    val pdp = new PDP(naturalPolicy, finder)
    
    for (n <- 0 until nbWarmups) {
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

    println(f"Evaluation - STAPL - ehealth - ${timer.mean / nbEvaluationsPerRun}%1.6f ms ($nbWarmups warmups, $nbRuns runs, $nbEvaluationsPerRun evaluations per run)")
  }

  def runEdocsTests(nbWarmups: Int, nbRuns: Int, nbEvaluationsPerRun: Int) = {
    import EdocsPolicy._

    val finder = new AttributeFinder
    finder += new ArtificialAttributeFinderModule
    val pdp = new PDP(policy, finder)
    
    for (n <- 0 until nbWarmups) {
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

    println(f"Evaluation - STAPL - edocs - ${timer.mean / nbEvaluationsPerRun}%1.6f ms ($nbWarmups warmups, $nbRuns runs, $nbEvaluationsPerRun evaluations per run)")
  }

  def runArtificialTests(size: Int, nbWarmups: Int, nbRuns: Int, nbEvaluationsPerRun: Int) = {
    val parser = new PolicyParser
    parser.addImport("stapl.core.tests.performance.ArtificialAttributes._")
    val policySizes = List(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200)
    if(! policySizes.contains(size)) {
      throw new RuntimeException(f"Invalid policy size: $size. Valid policy sizes: $policySizes")
    }
    val policyFile = policyHome + "/large-policy-l1-p" + size + "-a20.stapl"
    val p = parser.parseFile(policyFile)
    runArtificialTest(f"$size%d", p, nbWarmups, nbRuns, nbEvaluationsPerRun)
  }

  def runArtificialTest(label: String, policy: AbstractPolicy, nbWarmups: Int, nbRuns: Int, nbEvaluationsPerRun: Int) = {
    val finder = new AttributeFinder
    finder += new ArtificialAttributeFinderModule
    val pdp = new PDP(policy, finder)
    
    for (n <- 0 until nbWarmups) {
      for (n <- 0 until nbEvaluationsPerRun) {
	    val result = pdp.evaluate("maarten", "view", "doc123")
	    if (result.decision != NotApplicable) {
	      throw new RuntimeException("The policy did not evaluate correctly!")
	    }
	  }
    }

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

    println(f"Evaluation - STAPL - $label - ${timer.mean / nbEvaluationsPerRun}%1.6f ms ($nbWarmups warmups, $nbRuns runs, $nbEvaluationsPerRun evaluations per run)")
  }

}