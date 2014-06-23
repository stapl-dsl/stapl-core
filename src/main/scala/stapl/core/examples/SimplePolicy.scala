package stapl.core.examples

import stapl.core._

object Example extends App {

  val action = new AttributeContainer(ACTION)
  val subject = new AttributeContainer(SUBJECT)
  val resource = new AttributeContainer(RESOURCE)
  val env = new AttributeContainer(ENVIRONMENT)
  
  subject.roles = ListAttribute(String)
  subject.birthday = SimpleAttribute(Day)
  val student = subject.refine()
  student.group = SimpleAttribute(String)
  val teacher = subject.refine()
  teacher.isHeadTeacher = SimpleAttribute(Bool)
  resource.category = SimpleAttribute(String)
  env.today = SimpleAttribute(Day)
  
  // subject.attribute = SimpleAttribute("AlternativeName", Number)
  // val parent = subject.refine()
  // val child = subject.refine()
  
  // Possible types: String, 
  //                 Number, 
  //                 Bool, 
  //                 DateTime, 
  //                 Day, 
  //                 Time, 
  //                 DateTimeDuration,
  //                 DayDuration,
  //                 TimeDuration
  
  // Possible operations: String + String -> String
  //                      Number +|-|*|/ Number -> Number
  //                      abs(Number) -> Number
  //                      DateTime - DateTime -> DateTimeDuration
  //                      Day - Day -> DayDuration
  //                      Time - Time -> TimeDuration
  //                      DateTime +|- DateTimeDuration -> DateTime
  //                      DateTime +|- TimeDuration -> DateTime
  //                      DateTime +|- DayDuration -> DateTime
  //                      Day +|- DayDuration -> Day
  //                      Time +|- TimeDuration -> Time
  //                      DateTimeDuration +|- DateTimeDuration -> DateTimeDuration
  //                      DateTimeDuration +|- DayDuration -> DateTimeDuration
  //                      DateTimeDuration +|- TimeDuration -> DateTimeDuration
  //                      DayDuration +|- DayDuration -> DayDuration
  //                      DayDuration +|- TimeDuration -> DateTimeDuration
  //                      TimeDuration +|- TimeDuration -> TimeDuration
  
  // Construct dates: DateTime(year,month,day,hour,minute,second,millisecond)
  //                  DateTime(year,month,day,hour,minute,second)
  //                  Day(year,month,day)
  //                  Time(hour,minute,second,millisecond)
  //                  Time(hour,minute,second)
  
  // Construct durations: DateTimeDuration(years,months,days,hours,minutes,seconds,milliseconds)
  //                      DateTimeDuration(years,months,days,hours,minutes,seconds)
  //                      DayDuration(years,months,days)
  //                      TimeDuration(hours,minutes,seconds,milliseconds)
  //                      TimeDuration(hours,minutes,seconds)
  // or in an expression in a policy: 5.months + 3.days + 2.minutes + 20.millis
  
  val studentPolicy = 
    new Policy("policy2")(
        target = "student" in subject.roles,
        effect = Permit,
        condition = (subject.birthday + 18.years + 1.days) lteq env.today
    )
  
  def alwaysPermitRole(id: String, role: String) =
    new Policy(id)(
        target = role in subject.roles,
        effect = Permit
    )
  
  val policy = 
    
  new PolicySet("policyset1")(
      target = action.id === "access" & resource.category === "book",
      alwaysPermitRole("policy1", "teacher"),
      studentPolicy,
      defaultDeny("policy3")
  ) with FirstApplicable
  
  
  class MyModule extends AttributeFinderModule {
    
    override def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType): Option[ConcreteValue] = {
      cType match {
        case SUBJECT => name match {
          case "roles" => ctx.subjectId match {
            case "Jasper" => Some(List("student"))
            case "Sara" => Some(List("student"))
            case "Paul" => Some(List("teacher"))
            case _ => None
          }
          case "birthday" => ctx.subjectId match {
            case "Jasper" => Some(Day(1990, 2, 21))
            case "Sara" => Some(Day(1999, 3, 14))
            case "Paul" => Some(Day(1956, 7, 16))
            case _ => None
          }
          case _ => None
        }
        case RESOURCE => name match {
          case "category" => ctx.resourceId match {
            case "The Catcher in the Rye" => Some("book")
            case "Worst Case Scenario" => Some("cd")
            case _ => None
          }
          case _ => None
        }
        case ENVIRONMENT => name match {
          case "today" => {
            val date = new java.util.Date
            Some(Day(1900 + date.getYear(), date.getMonth() + 1, date.getDate()))
          }
          case _ => None
        }
        case _ => None
      }
    }
  }
  
  val finder = new AttributeFinder
  finder += new MyModule
  val pdp = new PDP(policy, finder)
  
  println(pdp.evaluate(new RequestCtx("Jasper", "access", "The Catcher in the Rye")))
}