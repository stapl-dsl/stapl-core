/*
 * Copyright 2015 Jasper Moeys, iMinds-DistriNet, KU Leuven
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package stapl.core

import stapl.core.typeclasses.Instances
import stapl.core.syntax.Syntax
import org.joda.time.Period
import org.joda.time.LocalDateTime
import org.joda.time.LocalDate
import org.joda.time.LocalTime

package object dsl extends DSL with Instances with Syntax {
  
  object any2stringadd // DIE any2stringadd DIE !!!
  
  /**
   * You might want to manually wrap something in a Value. Use with care.
   */
  @inline def Value[T](something: T) = stapl.core.Value.apply[T](something)
  

  implicit class DurationBuilder(val number: Int) extends AnyVal {
  
    def years = Period.years(number)
    
    def months = Period.months(number)
    
    def days = Period.days(number)
    
    def hours = Period.hours(number)
    
    def minutes = Period.minutes(number)
    
    def seconds = Period.seconds(number)
    
    def millis = Period.millis(number)
  }
  
  def DateTime(year: Int, month: Int, day: Int, hours: Int, minutes: Int, seconds: Int, millis: Int) = 
    new LocalDateTime(year, month, day, hours, minutes, seconds, millis)
  def DateTime(year: Int, month: Int, day: Int, hours: Int, minutes: Int, seconds: Int): LocalDateTime = 
    DateTime(year, month, day, hours, minutes, seconds, 0)
  def Day(year: Int, month: Int, day: Int) =
    new LocalDate(year, month, day)
  def Time(hours: Int, minutes: Int, seconds: Int, millis: Int) = 
    new LocalTime(hours, minutes, seconds, millis)
  def Time(hours: Int, minutes: Int, seconds: Int): LocalTime =
    Time(hours, minutes, seconds, 0)
  
  
  private[dsl] abstract class SubjectTemplate extends AttributeContainer(SUBJECT) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class ResourceTemplate extends AttributeContainer(RESOURCE) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class ActionTemplate extends AttributeContainer(ACTION) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class EnvironmentTemplate extends AttributeContainer(ENVIRONMENT)
  
  trait Subject extends SubjectTemplate
  trait Resource extends ResourceTemplate
  trait Action extends ActionTemplate
  trait Environment extends EnvironmentTemplate
}
