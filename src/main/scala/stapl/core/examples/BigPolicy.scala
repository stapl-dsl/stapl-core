package stapl.core.examples

import stapl.core._

object BigPolicy extends App {
  
  val action = new AttributeContainer(ACTION)
  val subject = new AttributeContainer(SUBJECT)
  val resource = new AttributeContainer(RESOURCE)
  val env = new AttributeContainer(ENVIRONMENT)
  
  env.currentDateTime                     = SimpleAttribute(DateTime)
  resource.type_                          = SimpleAttribute(String)
  resource.owner_withdrawn_consents       = ListAttribute(String)
  resource.operator_triggered_emergency   = SimpleAttribute(Bool)
  resource.indicates_emergency            = SimpleAttribute(Bool)
  resource.owner_id                       = SimpleAttribute("owner:id", String)
  resource.owner_responsible_physicians   = ListAttribute("owner:responsible_physicians", String)
  resource.owner_discharged               = SimpleAttribute("owner:discharged", Bool)
  resource.owner_discharged_dateTime      = SimpleAttribute("owner:discharged_dateTime", DateTime)
  resource.patient_status                 = SimpleAttribute(String)
  resource.created                        = SimpleAttribute(DateTime)
  subject.roles                           = ListAttribute(String)
  subject.triggered_breaking_glass        = SimpleAttribute(Bool)
  subject.department                      = SimpleAttribute(String)
  subject.current_patient_in_consultation = SimpleAttribute(String)
  subject.treated_in_last_six_months      = ListAttribute(String)
  subject.primary_patients                = ListAttribute(String)
  subject.is_head_physician               = SimpleAttribute(Bool)
  subject.treated                         = ListAttribute(String)
  subject.treated_by_team                 = ListAttribute(String)
  subject.admitted_patients_in_care_unit  = ListAttribute(String)
  subject.shift_start                     = SimpleAttribute(DateTime)
  subject.shift_stop                      = SimpleAttribute(DateTime)
  subject.location                        = SimpleAttribute(String)
  subject.admitted_patients_in_nurse_unit = ListAttribute(String)
  subject.allowed_to_access_pms           = SimpleAttribute(Bool)
  subject.responsible_patients            = ListAttribute(String)
  
  
  val policyTree = 
    
  new PolicySet("jisa13-final3")( // The policy set for "view patient status".
      target = action.id === "view" & resource.type_ === "patientstatus",
      new Policy("policy:1")( // The consent policy.
          target = "medical_personnel" in subject.roles,
          effect = Deny,
          condition = !subject.triggered_breaking_glass & (subject.id in resource.owner_withdrawn_consents)
      ),
      new Policy("policy:2")( // Only physicians, nurses and patients can access the monitoring system.
          effect = Deny,
          condition = !(("nurse" in subject.roles)  | ("physician" in subject.roles) | ("patient" in subject.roles))
      ),
      new PolicySet("policyset:2")( // For physicians.
          target = "physician" in subject.roles,
          new Policy("policy:3")( // Of the physicians, only gps, physicians of the cardiology department, physicians of the elder care department and physicians of the emergency department can access the monitoring system.
              effect = Deny,
              condition = !((subject.department === "cardiology")  | (subject.department === "elder_care") | (subject.department === "emergency") | ("gp" in subject.roles))
          ),
          new Policy("policy:4")( // All of the previous physicians can access the monitoring system in case of emergency.
              target = (subject.department === "cardiology")  | (subject.department === "elder_care") | (subject.department === "emergency"),
              effect = Permit,
              condition = subject.triggered_breaking_glass | resource.operator_triggered_emergency | resource.indicates_emergency
          ),
          new PolicySet("policyset:3")( // For GPs.
              target = "gp" in subject.roles,
              new Policy("policy:5")( // Permit if in consultation or treated in the last six months or primary physician or responsible in the system.
                  effect = Permit,
                  condition = (resource.owner_id === subject.current_patient_in_consultation) | (resource.owner_id in subject.treated_in_last_six_months) | (resource.owner_id in subject.primary_patients) | (subject.id in resource.owner_responsible_physicians)
              ),
              defaultDeny("policy:6")
          ) with PermitOverrides,
          new PolicySet("policyset:4")( // For cardiologists.
              target = subject.department === "cardiology",
              new Policy("policy:7")( // Permit for head physician.
                  target = subject.is_head_physician,
                  effect = Permit
              ),
              new Policy("policy:8")( // Permit if treated the patient or treated in team.
                  effect = Permit,
                  condition = (resource.owner_id in subject.treated) | (resource.owner_id in subject.treated_by_team)
              ),
              defaultDeny("policy:9")
          ) with PermitOverrides,
          new PolicySet("policyset:5")( // For physicians of elder care department
              target = subject.department === "elder_care",
              new Policy("policy:10")( // Permit if admitted in care unit or treated in the last six months.
                  effect = Permit,
                  condition = (resource.owner_id in subject.admitted_patients_in_care_unit) | (resource.owner_id in subject.treated_in_last_six_months)
              ),
              defaultDeny("policy:11")
          ) with PermitOverrides,
          new PolicySet("policyset:6")( // For physicians of emergency department
              target = subject.department === "emergency",
              new Policy("policy:12")( // Permit if patient status is bad (or the above).
                  effect = Permit,
                  condition = resource.patient_status === "bad"
              ),
              defaultDeny("policy:13")
          ) with PermitOverrides
      ) with FirstApplicable,
      new PolicySet("policyset:7")( // For nurses.
          target = "nurse" in subject.roles,
          new Policy("policy:14")( // Of the nurses, only nurses of the cardiology department or the elder care department can access the PMS.
              effect = Deny,
              condition = !((subject.department === "cardiology") | (subject.department === "elder_care"))
          ),
          new Policy("policy:15")( // Nurses can only access the PMS during their shifts.
              effect = Deny,
              condition = !((env.currentDateTime gteq subject.shift_start) & (env.currentDateTime lteq subject.shift_stop))
          ),
          new Policy("policy:16")( // Nurses can only access the PMS from the hospital.
              effect = Deny,
              condition = !(subject.location === "hospital")
          ),
          new Policy("policy:17")( // Nurses can only view the patient's status of the last five days.
              effect = Deny,
              condition = !(env.currentDateTime lteq (resource.created + 5.days))
          ),
          new PolicySet("policyset:8")( // For nurses of emergency department.
              target = subject.department === "cardiology",
              new Policy("policy:18")( // Nurses of the cardiology department can only view the patient status of a patient in their nurse unit for whom they are assigned responsible, up to three days after they were discharged.
                  target = subject.department === "cardiology",
                  effect = Permit,
                  condition = (resource.owner_id in subject.admitted_patients_in_nurse_unit) & (!resource.owner_discharged | (env.currentDateTime lteq (resource.owner_discharged_dateTime + 3.days)))
              ),
              defaultDeny("policy:19")
          ) with PermitOverrides,
          new PolicySet("policyset:9")( // For nurses of elder care department.
              target = subject.department === "elder_care",
              new Policy("policy:20")( // Of the nurses of the elder care department, only nurses who have been allowed to use the PMS can access the PMS.
                  effect = Deny,
                  condition = !subject.allowed_to_access_pms
              ),
              new PolicySet("policySet:10")( // Nurses of the elder care department can only view the patient status of a patient who is currently admitted to their nurse unit and for whome they are assigned responsible.
                  target = true,
                  new Policy("policy:21")( // Nurses of the elder care department can only view the patient status of a patient who is currently admitted to their nurse unit and for whome they are assigned responsible.
                      effect = Permit,
                      condition = (resource.owner_id in subject.admitted_patients_in_nurse_unit) & (resource.owner_id in subject.responsible_patients)
                  ),
                  defaultDeny("policy:22")
              ) with PermitOverrides
          ) with DenyOverrides
      ) with FirstApplicable,
      new PolicySet("policyset:11")( // For patients
          target = "patient" in subject.roles,
          new Policy("policy:23")( // A patient can only access the PMS if (still) allowed by the hospital (e.g., has subscribed to the PMS, but is not paying any more).
              effect = Deny,
              condition = !subject.allowed_to_access_pms
          ),
          new Policy("policy:24")( // A patient can only view his own status.
              effect = Deny,
              condition = !(resource.owner_id === subject.id)
          ),
          defaultPermit("policy:25")
      ) with FirstApplicable
  ) with DenyOverrides
  
  println(policyTree)
}