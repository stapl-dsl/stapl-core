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
import stapl.core.pdp.PDP
import stapl.core.pdp.RequestCtx
import stapl.core.pdp.EvaluationCtx
import stapl.core.pdp.AttributeFinderModule
import stapl.core.pdp.AttributeFinder

/**
 * An example of how to specify and use a policy:
 * 1. attribute definitions
 * 2. policy specification
 * 3. implementation of an attribute finder (hard-coded values)
 * 4. set-up of the PDP
 * 5. requesting decisions from the PDP
 */
object Example extends App with BasicPolicy {
  
  // 1. attribute definitions
  subject.roles = ListAttribute(String)
  subject.birthday = SimpleAttribute(Day)
  val student = subject.refine()
  student.group = SimpleAttribute(String)
  val teacher = subject.refine()
  teacher.isHeadTeacher = SimpleAttribute(Bool)
  resource.category = SimpleAttribute(String)
  environment.today = SimpleAttribute(Day)
  
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
  
  
  // 2. policy specification
  val studentPolicy = 
    new Rule("policy2")(
        target = "student" in subject.roles,
        effect = Permit,
        condition = (subject.birthday + 18.years + 1.days) lteq environment.today
    )
  
  def alwaysPermitRole(id: String, role: String) =
    new Rule(id)(
        target = role in subject.roles,
        effect = Permit
    )
  
  val policy = Policy("policyset1") := when (action.id === "access" & resource.category === "book") apply FirstApplicable to (
      alwaysPermitRole("policy1", "teacher"),
      studentPolicy,
      Rule("policy3") := deny
  )
  
  // 3. implementation of an attribute finder (hard-coded values)
  class MyModule extends AttributeFinderModule {
    
    override def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, 
        aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
      // Note: we do not check the multiValued boolean here, the code below
      // knows these hard-coded
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
  
  // 4. set-up of the PDP
  val finder = new AttributeFinder
  finder += new MyModule
  val pdp = new PDP(policy, finder)
  
  // 5. requesting decisions from the PDP
  println(pdp.evaluate(new RequestCtx("Jasper", "access", "The Catcher in the Rye")))
}