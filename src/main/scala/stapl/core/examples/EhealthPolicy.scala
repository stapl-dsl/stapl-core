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
 * Example of a policy based on an e-health scenario.
 */
object EhealthPolicy extends BasicPolicy {
  
  environment.currentDateTime = SimpleAttribute(DateTime)
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

  // The policy set for "view patient status".
  val naturalPolicy = Policy("ehealth") := when (action.id === "view" & resource.type_ === "patientstatus") apply DenyOverrides to (    
    // The consent policy.
    Policy("policy:1") := when ("medical_personnel" in subject.roles) apply PermitOverrides to (
        Rule("consent") := deny iff (subject.id in resource.owner_withdrawn_consents),
        Rule("breaking-glass") := permit iff (subject.triggered_breaking_glass) performing (log(subject.id + " performed breaking-the-glass procedure"))
    ) performing (log("permit because of breaking-the-glass procedure") on Permit),
    
    // Only physicians, nurses and patients can access the monitoring system.
    Rule("policy:2") := deny iff !(("nurse" in subject.roles) | ("physician" in subject.roles) | ("patient" in subject.roles)),
    
    // For physicians.
    Policy("policyset:2") := when ("physician" in subject.roles) apply FirstApplicable to (      
      // Of the physicians, only gps, physicians of the cardiology department, physicians of the elder care department and physicians of the emergency department can access the monitoring system.
      Rule("policy:3") := deny iff !((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency") | ("gp" in subject.roles)),
      
      // All of the previous physicians can access the monitoring system in case of emergency.
      Rule("policy:4") := when ((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency"))
        permit iff (subject.triggered_breaking_glass | resource.operator_triggered_emergency | resource.indicates_emergency),
      
      // For GPs: only permit if in consultation or treated in the last six months or primary physician or responsible in the system.
      OnlyPermitIff("policyset:3")(
          target = "gp" in subject.roles,
          (resource.owner_id === subject.current_patient_in_consultation)
          | (resource.owner_id in subject.treated_in_last_six_months)
          | (resource.owner_id in subject.primary_patients)
          | (subject.id in resource.owner_responsible_physicians)
      ),
      
      // For cardiologists.
      Policy("policyset:4") := when (subject.department === "cardiology") apply PermitOverrides to (        
        // Permit for head physician.
        Rule("policy:7") := when (subject.is_head_physician) permit,
        
        // Permit if treated the patient or treated in team.
        Rule("policy:8") := permit iff (resource.owner_id in subject.treated) | (resource.owner_id in subject.treated_by_team),
        
        Rule("policy:9") := deny
      ),
      
      // For physicians of elder care department: only permit if admitted in care unit or treated in the last six months.
      OnlyPermitIff("policyset:5")(
          target = subject.department === "elder_care",
          (resource.owner_id in subject.admitted_patients_in_care_unit)
          | (resource.owner_id in subject.treated_in_last_six_months)
      ),
      
      // For physicians of emergency department: only permit if patient status is bad (or the above).
      OnlyPermitIff("policyset:6")(
          target = subject.department === "emergency",   
          resource.patient_status === "bad"
      )
    ),
    
    // For nurses.
    Policy("policyset:7") := when ("nurse" in subject.roles) apply FirstApplicable to (      
      // Of the nurses, only nurses of the cardiology department or the elder care department can access the PMS.
      Rule("policy:14") := deny iff !((subject.department === "cardiology") | (subject.department === "elder_care")),
      
      // Nurses can only access the PMS during their shifts.
      Rule("policy:15") := deny iff !((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)),
      
      // Nurses can only access the PMS from the hospital.
      Rule("policy:16") := deny iff !(subject.location === "hospital"),
      
      // Nurses can only view the patient's status of the last five days.
      Rule("policy:17") := deny iff !(environment.currentDateTime lteq (resource.created + 5.days)),
      
      // For nurses of cardiology department: they can only view the patient status of a patient 
      // in their nurse unit for whom they are assigned responsible, up to three days after they were discharged.
      OnlyPermitIff("policyset:8")(
          target = subject.department === "cardiology",
          (resource.owner_id in subject.admitted_patients_in_nurse_unit) 
          	& (!resource.owner_discharged | (environment.currentDateTime lteq (resource.owner_discharged_dateTime + 3.days)))
      ),
        
      // For nurses of the elder care department.
      Policy("policyset:9") := when (subject.department === "elder_care") apply DenyOverrides to (
        // Of the nurses of the elder care department, only nurses who have been allowed to use the PMS can access the PMS.
        Rule("policy:20") := deny iff !subject.allowed_to_access_pms,
        
        // Nurses of the elder care department can only view the patient status of a patient 
        // who is currently admitted to their nurse unit and for whome they are assigned responsible.
        OnlyPermitIff("policySet:10")(
            target = AlwaysTrue,
            (resource.owner_id in subject.admitted_patients_in_nurse_unit) 
            	& (resource.owner_id in subject.responsible_patients)
        )
      )
    ),
    // For patients
    Policy("policyset:11") := when ("patient" in subject.roles) apply FirstApplicable to (      
	      // A patient can only access the PMS if (still) allowed by the hospital (e.g., has 
    	  // subscribed to the PMS, but is not paying any more).
	      Rule("policy:23") := deny iff !subject.allowed_to_access_pms,
	      
	      // A patient can only view his own status.
	      Rule("policy:24") := deny iff !(resource.owner_id === subject.id),
	      
	      Rule("policy:25") := permit
    )
  )
  
  val javaLikePolicy = new Policy("ehealth")(
    target = action.id === "view" & resource.type_ === "patientstatus",
    pca = DenyOverrides,   
    subpolicies = List(
	    // The consent policy.
	    new Policy("policy:1")(
	        target = "medical_personnel" in subject.roles,
	        pca = PermitOverrides,
	        subpolicies = List(
		        new Rule("consent")(
		            target = AlwaysTrue,
		            effect = Deny,
		            condition = subject.id in resource.owner_withdrawn_consents),
		        new Rule("breaking-glass")(
		            target = AlwaysTrue,
		            effect = Permit,
		            condition = subject.triggered_breaking_glass,
		            obligationActions = List(log(subject.id + " performed breaking-the-glass procedure")))
		    ),
		    obligations = List(
		        new Obligation(log("just another log on Permit"), Permit)
		    )
	    ),
	    
	    // Only physicians, nurses and patients can access the monitoring system.
	    new Rule("policy:2")(
	        target = AlwaysTrue,
	        effect = Deny,
	        condition = !(("nurse" in subject.roles) | ("physician" in subject.roles) | ("patient" in subject.roles))),
	    
	    // For physicians.
	    new Policy("policyset:2")(
	      target = "physician" in subject.roles,
	      pca = FirstApplicable,  
	      subpolicies = List(
		      // Of the physicians, only gps, physicians of the cardiology department, physicians of the elder care department and physicians of the emergency department can access the monitoring system.
		      new Rule("policy:3")(
		        target = AlwaysTrue,
		        effect = Deny,
		        condition = !((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency") | ("gp" in subject.roles))),
		      
		      // All of the previous physicians can access the monitoring system in case of emergency.
		      new Rule("policy:4")(
		        target = (subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency"),
		        effect = Permit,
		        condition = (subject.triggered_breaking_glass | resource.operator_triggered_emergency | resource.indicates_emergency)),
		      
		      // For GPs: only permit if in consultation or treated in the last six months or primary physician or responsible in the system.
		      OnlyPermitIff("policyset:3")(
		          target = "gp" in subject.roles,
		          (resource.owner_id === subject.current_patient_in_consultation)
		          | (resource.owner_id in subject.treated_in_last_six_months)
		          | (resource.owner_id in subject.primary_patients)
		          | (subject.id in resource.owner_responsible_physicians)
		      ),
		      
		      // For cardiologists.
		      new Policy("policyset:4")(
		        target = subject.department === "cardiology",
		        pca = PermitOverrides,  
		        subpolicies = List(
			        // Permit for head physician.
			        new Rule("policy:7")(
			            target = subject.is_head_physician,
			            effect = Permit),
			        
			        // Permit if treated the patient or treated in team.
			        new Rule("policy:8")(
			            target = (resource.owner_id in subject.treated) | (resource.owner_id in subject.treated_by_team),
			            effect = Permit),
			        
			        defaultDeny("policy:9")
			    )
		      ),
		      
		      // For physicians of elder care department: only permit if admitted in care unit or treated in the last six months.
		      OnlyPermitIff("policyset:5")(
		          target = subject.department === "elder_care",
		          (resource.owner_id in subject.admitted_patients_in_care_unit)
		          | (resource.owner_id in subject.treated_in_last_six_months)
		      ),
		      
		      // For physicians of emergency department: only permit if patient status is bad (or the above).
		      OnlyPermitIff("policyset:6")(
		          target = subject.department === "emergency",   
		          resource.patient_status === "bad"
		      )
		  )
	    ),
	    
	    // For nurses.
	    new Policy("policyset:7")(
	      target = "nurse" in subject.roles,
	      pca = FirstApplicable,    
	      subpolicies = List(
		      // Of the nurses, only nurses of the cardiology department or the elder care department can access the PMS.
		      new Rule("policy:14")(
		          target = !((subject.department === "cardiology") | (subject.department === "elder_care")),
		          effect = Deny),
		      
		      // Nurses can only access the PMS during their shifts.
		      new Rule("policy:15")(
		          target = !((environment.currentDateTime gteq subject.shift_start) & (environment.currentDateTime lteq subject.shift_stop)),
		          effect = Deny),
		      
		      // Nurses can only access the PMS from the hospital.
		      new Rule("policy:16")(
		          target = !(subject.location === "hospital"),
		          effect = Deny),
		      
		      // Nurses can only view the patient's status of the last five days.
		      new Rule("policy:17")(
		          target = !(environment.currentDateTime lteq (resource.created + 5.days)),
		          effect = Deny),
		      
		      // For nurses of cardiology department: they can only view the patient status of a patient 
		      // in their nurse unit for whom they are assigned responsible, up to three days after they were discharged.
		      OnlyPermitIff("policyset:8")(
		          target = subject.department === "cardiology",
		          (resource.owner_id in subject.admitted_patients_in_nurse_unit) 
		          	& (!resource.owner_discharged | (environment.currentDateTime lteq (resource.owner_discharged_dateTime + 3.days)))
		      ),
		        
		      // For nurses of the elder care department.
		      new Policy("policyset:9")(
		        target = subject.department === "elder_care",
		        pca = DenyOverrides,
		        subpolicies = List(
			        // Of the nurses of the elder care department, only nurses who have been allowed to use the PMS can access the PMS.
			        new Rule("policy:20")(
			            target = !subject.allowed_to_access_pms,
			            effect = Deny),
			        
			        // Nurses of the elder care department can only view the patient status of a patient 
			        // who is currently admitted to their nurse unit and for whome they are assigned responsible.
			        OnlyPermitIff("policySet:10")(
			            target = AlwaysTrue,
			            (resource.owner_id in subject.admitted_patients_in_nurse_unit) 
			            	& (resource.owner_id in subject.responsible_patients)
			        )
			    )
			  )
	      )
	    ),
	    // For patients
	    new Policy("policyset:11")(
	      target = "patient" in subject.roles,
	      pca = FirstApplicable,  
	      subpolicies = List(
		      // A patient can only access the PMS if (still) allowed by the hospital (e.g., has 
			  // subscribed to the PMS, but is not paying any more).
		      new Rule("policy:23")(
		          target = !subject.allowed_to_access_pms,
		          effect = Deny),
		      
		      // A patient can only view his own status.
		      new Rule("policy:24")(
		          target = !(resource.owner_id === subject.id),
		          effect = Deny),
		      
		      defaultPermit("policy:25")
		  )
	    )
	 )
  )
}