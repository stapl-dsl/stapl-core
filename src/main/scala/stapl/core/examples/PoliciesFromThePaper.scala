/**
 *    Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */
package stapl.core.examples

import stapl.core._
import stapl.core.templates._

/**
 * The policies from the paper (see X).
 * 
 * TODO add ref to the paper after publication
 */
object PoliciesFromThePaper extends BasicPolicy {
  
  	subject.roles = ListAttribute(String)
	subject.triggered_breaking_glass = SimpleAttribute(Bool)
	subject.department = SimpleAttribute(String)
	subject.treated = ListAttribute(String)
	val physician = subject.refine()	
	physician.is_head_physician = SimpleAttribute(Bool)
	val nurse = subject.refine()
	nurse.shift_start = SimpleAttribute(Time)
	nurse.shift_stop = SimpleAttribute(Time)
	resource.owner_id = SimpleAttribute(String)
	environment.currentTime = SimpleAttribute(Time)

  val bigExample = Policy("for-viewing") := when(action.id === "view") apply PermitOverrides to (
    // Permit head physicians
    Rule("head") := when("physician" in subject.roles) permit iff (physician.is_head_physician),

    // For nurses
    Policy("nurses") := when("nurse" in subject.roles) apply PermitOverrides to (

      // Permit cardiology nurses who have treated the patient that owns the resource
      Rule("treated") := when(nurse.department === "cardiology") permit iff (resource.owner_id in nurse.treated),

      // Nurses can only access the PMS during their shifts.
      Rule("shifts") := deny iff !((environment.currentTime gteq nurse.shift_start) & (environment.currentTime lteq nurse.shift_stop)),

      // Patients can only view their own data
      Rule("patients") := when ("patient" in subject.roles) deny
        iff (! (subject.id === resource.owner_id)),

      // Deny otherwise
      Rule("default-deny") := deny
    ),

    // Permit anyone to view the data if they have triggered the breaking-the-glass procedure, but log this
    Rule("glass") := permit iff (subject.triggered_breaking_glass) performing (log(subject.id + " broke the glass"))
  )
}