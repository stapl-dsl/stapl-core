package stapl.core.examples

import stapl.core._
import stapl.core.templates._
import stapl.core.pdp.PDP

/**
 * The policy from readme.md
 */
object PolicyFromTheReadMe extends BasicPolicy {
 
  subject.roles								= ListAttribute(String)
  subject.treated							= ListAttribute(String)
  resource.type_ 							= SimpleAttribute(String)
  resource.owner_id							= SimpleAttribute(String)
  // action.id is defined by STAPL itself
  
  // Permit only if the physician treated the owner of the patient data.
  val policy = Policy("e-health example") := when ((action.id === "view") 
		  & (resource.type_ === "patient-data") 
		  & ("physician" in subject.roles)) apply PermitOverrides to (
      Rule("requirement-for-permit") := permit iff (resource.owner_id in subject.treated),
      defaultDeny
  )
  
}
object App {
  
  def main(args: Array[String]) {
	  import PolicyFromTheReadMe._
	  val pdp = new PDP(policy)
	  println(pdp.evaluate("subject1", "view", "resource1", 
	      subject.roles -> List("physician"),
	      subject.treated -> List("patientX"),
	      resource.type_ -> "patient-data",
	      resource.owner_id -> "patientX"))
  }
}