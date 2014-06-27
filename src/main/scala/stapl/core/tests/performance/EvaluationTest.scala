package stapl.core.tests.performance

import stapl.core.examples.EhealthPolicy
import stapl.core.pdp.PDP
import stapl.core.pdp.AttributeFinder
import org.joda.time.LocalDateTime
import stapl.core.Result
import stapl.core.Deny
import stapl.core.AbstractPolicy

object EvaluationTest extends App {

  import EhealthPolicy._

  def evaluateNurseRequest(pdp: PDP): Result = {
    pdp.evaluate("maarten", "view", "doc123",
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
  }

  def runTests(label: String, policy: AbstractPolicy, nbRuns: Int = 1000) = {
    println("================================================")
    println(f"Starting test $label ($nbRuns%d runs)")
    println("================================================")

    val pdp = new PDP(policy, new AttributeFinder)

    val timer = new Timer

    // then run the tests
    for (n <- 0 until nbRuns) {
      timer.time {
        for (n <- 0 until 1000) {
          val result = evaluateNurseRequest(pdp)
          if (result.decision != Deny) {
            throw new RuntimeException("The policy did not evaluate correctly!")
          }
        }
      }
    }

    println(f"Mean evaluation time of 1000 evaluations: ${timer.mean}%1.2f ms (stdDev: ${timer.stdDev}%1.2f, confInt: ${timer.confInt()}%1.2f)")
  }

  runTests("Natural policy", naturalPolicy)
  runTests("Java-like policy", javaLikePolicy)

}