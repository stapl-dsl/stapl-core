package stapl.core.examples

import stapl.core._
import stapl.core.templates._

object EhealthPolicy {

  val subject = stapl.core.subject // FIXME do we work on the single subject object here? we need a local copy of some sort
  val resource = stapl.core.resource
  val action = stapl.core.action
  val env = stapl.core.environment

  env.currentDateTime = SimpleAttribute(DateTime)
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
  val policy = new PolicySet("jisa13-final3")(
    target = action.id === "view" & resource.type_ === "patientstatus",
    pca = DenyOverrides,
    
    // The consent policy.
    Policy("policy:1") := when ("medical_personnel" in subject.roles) deny iff (!subject.triggered_breaking_glass & (subject.id in resource.owner_withdrawn_consents)),
    
    // Only physicians, nurses and patients can access the monitoring system.
    Policy("policy:2") := deny iff !(("nurse" in subject.roles) | ("physician" in subject.roles) | ("patient" in subject.roles)),
    
    // For physicians.
    new PolicySet("policyset:2")(
      target = "physician" in subject.roles,
      pca = FirstApplicable,
      
      // Of the physicians, only gps, physicians of the cardiology department, physicians of the elder care department and physicians of the emergency department can access the monitoring system.
      Policy("policy:3") := deny iff !((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency") | ("gp" in subject.roles)),
      
      // All of the previous physicians can access the monitoring system in case of emergency.
      Policy("policy:4") := when ((subject.department === "cardiology") | (subject.department === "elder_care") | (subject.department === "emergency"))
        permit iff (subject.triggered_breaking_glass | resource.operator_triggered_emergency | resource.indicates_emergency),
      
      // For GPs.
      new PolicySet("policyset:3")(
        target = "gp" in subject.roles,
        pca = PermitOverrides,
        
        // Permit if in consultation or treated in the last six months or primary physician or responsible in the system.
        Policy("policy:5") := permit iff ((resource.owner_id === subject.current_patient_in_consultation) | (resource.owner_id in subject.treated_in_last_six_months) | (resource.owner_id in subject.primary_patients) | (subject.id in resource.owner_responsible_physicians)),
        
        defaultDeny("policy:6")),
      // For cardiologists.
      new PolicySet("policyset:4")(
        target = subject.department === "cardiology",
        pca = PermitOverrides,
        
        // Permit for head physician.
        Policy("policy:7") := when (subject.is_head_physician) permit,
        
        // Permit if treated the patient or treated in team.
        Policy("policy:8") := permit iff (resource.owner_id in subject.treated) | (resource.owner_id in subject.treated_by_team),
        
        defaultDeny("policy:9")),
      // For physicians of elder care department
      new PolicySet("policyset:5")(
        target = subject.department === "elder_care",
        pca = PermitOverrides,
        
        // Permit if admitted in care unit or treated in the last six months.
        Policy("policy:10") := permit iff (resource.owner_id in subject.admitted_patients_in_care_unit) | (resource.owner_id in subject.treated_in_last_six_months),

        defaultDeny("policy:11")),
      // For physicians of emergency department
      new PolicySet("policyset:6")(
        target = subject.department === "emergency",
        pca = PermitOverrides,
        
        // Permit if patient status is bad (or the above).
        Policy("policy:12") := permit iff resource.patient_status === "bad",
        
        defaultDeny("policy:13"))),
    // For nurses.
    new PolicySet("policyset:7")(
      target = "nurse" in subject.roles,
      pca = FirstApplicable,
      
      // Of the nurses, only nurses of the cardiology department or the elder care department can access the PMS.
      Policy("policy:14") := deny iff !((subject.department === "cardiology") | (subject.department === "elder_care")),
      
      // Nurses can only access the PMS during their shifts.
      Policy("policy:15") := deny iff !((env.currentDateTime gteq subject.shift_start) & (env.currentDateTime lteq subject.shift_stop)),
      
      // Nurses can only access the PMS from the hospital.
      Policy("policy:16") := deny iff !(subject.location === "hospital"),
      
      // Nurses can only view the patient's status of the last five days.
      Policy("policy:17") := deny iff !(env.currentDateTime lteq (resource.created + 5.days)),
      
      // For nurses of cardiology department.
      new PolicySet("policyset:8")( // TODO make this into a reusable pattern
        target = subject.department === "cardiology",
        pca = PermitOverrides,
        
        // Nurses of the cardiology department can only view the patient status of a patient in their nurse unit for whom they are assigned responsible, up to three days after they were discharged.
        Policy("policy:18") := when (subject.department === "cardiology") permit iff (resource.owner_id in subject.admitted_patients_in_nurse_unit) & (!resource.owner_discharged | (env.currentDateTime lteq (resource.owner_discharged_dateTime + 3.days))),

        Policy("policy:19") := deny),
      // For nurses of the elder care department.
      new PolicySet("policyset:9")(
        target = subject.department === "elder_care",
        pca = DenyOverrides,
        
        // Of the nurses of the elder care department, only nurses who have been allowed to use the PMS can access the PMS.
        Policy("policy:20") := deny iff !subject.allowed_to_access_pms,
        
        // Nurses of the elder care department can only view the patient status of a patient who is currently admitted to their nurse unit and for whome they are assigned responsible.
        new PolicySet("policySet:10")( // TODO make this a pattern onlyPermitIf
          target = true,
          pca = PermitOverrides,
          
          Policy("policy:21") := permit iff (resource.owner_id in subject.admitted_patients_in_nurse_unit) & (resource.owner_id in subject.responsible_patients),

          defaultDeny("policy:22")))),
    // For patients
    new PolicySet("policyset:11")(
      target = "patient" in subject.roles,
      pca = FirstApplicable,
      
      // A patient can only access the PMS if (still) allowed by the hospital (e.g., has subscribed to the PMS, but is not paying any more).
      Policy("policy:23") := deny iff !subject.allowed_to_access_pms,
      
      // A patient can only view his own status.
      Policy("policy:24") := deny iff !(resource.owner_id === subject.id),
      
      Policy("policy:25") := permit))
}