package stapl.test

import org.joda.time.LocalDateTime

object FullPolicyTest extends Templates {

  import stapl.core.dsl._
  
  object env extends Environment {
    val currentDateTime = Attribute[LocalDateTime]("currentDateTime")
  }
  object res extends Resource {
    val type_ = Attribute[String]("type")
    val owner_withdrawn_consents = Attribute[List[String]]("owner_withdrawn_consents")
    val operator_triggered_emergency = Attribute[Boolean]("operator_triggered_emergency")
    val indicates_emergency = Attribute[Boolean]("indicates_emergency")
    val owner_id = Attribute[String]("owner:id")
    val owner_responsible_physicians = Attribute[List[String]]("owner:responsible_physicians")
    val owner_discharged = Attribute[Boolean]("owner:discharged")
    val owner_discharged_dateTime = Attribute[LocalDateTime]("owner:discharged_dateTime")
    val patient_status = Attribute[String]("patient_status")
    val created = Attribute[LocalDateTime]("created")
  }
  object sub extends Subject {
    val roles = Attribute[List[String]]("roles")
    val triggered_breaking_glass = Attribute[Boolean]("triggered_breaking_glass")
    val department = Attribute[String]("department")
    val current_patient_in_consultation = Attribute[String]("current_patient_in_consultation")
    val treated_in_last_six_months = Attribute[List[String]]("treated_in_last_six_months")
    val primary_patients = Attribute[List[String]]("primary_patients")
    val is_head_physician = Attribute[Boolean]("is_head_physician")
    val treated = Attribute[List[String]]("treated")
    val treated_by_team = Attribute[List[String]]("treated_by_team")
    val admitted_patients_in_care_unit = Attribute[List[String]]("admitted_patients_in_care_unit")
    val shift_start = Attribute[LocalDateTime]("shift_start")
    val shift_stop = Attribute[LocalDateTime]("shift_stop")
    val location = Attribute[String]("location")
    val admitted_patients_in_nurse_unit = Attribute[List[String]]("admitted_patients_in_nurse_unit")
    val allowed_to_access_pms = Attribute[Boolean]("allowed_to_access_pms")
    val responsible_patients = Attribute[List[String]]("responsible_patients")
  }
  object act extends Action
  
  val ehealthPolicy =
    
  Policy("ehealth") := when (act.id === "view" & res.type_ === "patientstatus") apply DenyOverrides to (    
    // The consent policy.
    Policy("policy:1") := when ("medical_personnel" in sub.roles) apply PermitOverrides to (
        Rule("consent") := deny iff (sub.id in res.owner_withdrawn_consents),
        Rule("breaking-glass") := permit iff (sub.triggered_breaking_glass) performing (log(sub.id + " performed breaking-the-glass procedure"))
    ) performing (log("permit because of breaking-the-glass procedure") on Permit),
    
    // Only physicians, nurses and patients can access the monitoring system.
    Rule("policy:2") := deny iff !(("nurse" in sub.roles) | ("physician" in sub.roles) | ("patient" in sub.roles)),
    
    // For physicians.
    Policy("policyset:2") := when ("physician" in sub.roles) apply FirstApplicable to (      
      // Of the physicians, only gps, physicians of the cardiology department, physicians of the elder care department and physicians of the emergency department can access the monitoring system.
      Rule("policy:3") := deny iff !((sub.department === "cardiology") | (sub.department === "elder_care") | (sub.department === "emergency") | ("gp" in sub.roles)),
      
      // All of the previous physicians except for the GPs can access the monitoring system in case of emergency.
      Rule("policy:4") := permit iff (((sub.department === "cardiology") | (sub.department === "elder_care") | (sub.department === "emergency"))
                                      & (sub.triggered_breaking_glass | res.operator_triggered_emergency | res.indicates_emergency)),
      
      // For GPs: only permit if in consultation or treated in the last six months or primary physician or responsible in the system.
      OnlyPermitIff("policyset:3")("gp" in sub.roles)(
          (res.owner_id === sub.current_patient_in_consultation)
          | (res.owner_id in sub.treated_in_last_six_months)
          | (res.owner_id in sub.primary_patients)
          | (sub.id in res.owner_responsible_physicians)
      ),
      
      // For cardiologists.
      Policy("policyset:4") := when (sub.department === "cardiology") apply PermitOverrides to (        
        // Permit for head physician.
        Rule("policy:7") := permit iff (sub.is_head_physician),
        
        // Permit if treated the patient or treated in team.
        Rule("policy:8") := permit iff (res.owner_id in sub.treated) | (res.owner_id in sub.treated_by_team),
        
        Rule("policy:9") := deny
      ),
      
      // For physicians of elder care department: only permit if admitted in care unit or treated in the last six months.
      OnlyPermitIff("policyset:5")(sub.department === "elder_care")(
          (res.owner_id in sub.admitted_patients_in_care_unit)
          | (res.owner_id in sub.treated_in_last_six_months)
      ),
      
      // For physicians of emergency department: only permit if patient status is bad (or the above).
      OnlyPermitIff("policyset:6")(sub.department === "emergency")(   
          res.patient_status === "bad"
      )
    ),
    
    // For nurses.
    Policy("policyset:7") := when ("nurse" in sub.roles) apply FirstApplicable to (      
      // Of the nurses, only nurses of the cardiology department or the elder care department can access the PMS.
      Rule("policy:14") := deny iff !((sub.department === "cardiology") | (sub.department === "elder_care")),
      
      // Nurses can only access the PMS during their shifts.
      Rule("policy:15") := deny iff !((env.currentDateTime >= sub.shift_start) & (env.currentDateTime <= sub.shift_stop)),
      
      // Nurses can only access the PMS from the hospital.
      Rule("policy:16") := deny iff !(sub.location === "hospital"),
      
      // Nurses can only view the patient's status of the last five days.
      Rule("policy:17") := deny iff !(env.currentDateTime lteq res.created + 5.days),
      
      // For nurses of cardiology department: they can only view the patient status of a patient 
      // in their nurse unit for whom they are assigned responsible, up to three days after they were discharged.
      OnlyPermitIff("policyset:8")(sub.department === "cardiology")(
          (res.owner_id in sub.admitted_patients_in_nurse_unit) 
      ),
        
      // For nurses of the elder care department.
      Policy("policyset:9") := when (sub.department === "elder_care") apply DenyOverrides to (
        // Of the nurses of the elder care department, only nurses who have been allowed to use the PMS can access the PMS.
        Rule("policy:20") := deny iff !sub.allowed_to_access_pms,
        
        // Nurses of the elder care department can only view the patient status of a patient 
        // who is currently admitted to their nurse unit and for whome they are assigned responsible.
        OnlyPermitIff("policySet:10")(true)(
            (res.owner_id in sub.admitted_patients_in_nurse_unit) 
              & (res.owner_id in sub.responsible_patients)
        )
      )
    ),
    // For patients
    Policy("policyset:11") := when ("patient" in sub.roles) apply FirstApplicable to (      
        // A patient can only access the PMS if (still) allowed by the hospital (e.g., has 
        // subscribed to the PMS, but is not paying any more).
        Rule("policy:23") := deny iff !sub.allowed_to_access_pms,
        
        // A patient can only view his own status.
        Rule("policy:24") := deny iff !(res.owner_id === sub.id),
        
        Rule("policy:25") := permit
    )
  )
  
  def main(args: Array[String]) {
    print(ehealthPolicy)
    println()
    print(sub.roles)
  }

}