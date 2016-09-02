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

package stapl.core.typeclasses.impl

import org.joda.time.Period
import org.joda.time.LocalDate
import org.joda.time.LocalDateTime
import org.joda.time.LocalTime
import org.joda.time.base.BaseLocal
import scala.annotation.implicitNotFound
import stapl.core.typeclasses.Absable
import stapl.core.typeclasses.Addable
import stapl.core.typeclasses.Comparable
import stapl.core.typeclasses.Containable
import stapl.core.typeclasses.Divisible
import stapl.core.typeclasses.Equatable
import stapl.core.typeclasses.Multipliable
import stapl.core.typeclasses.Subtractable

trait Instances {
  //implicit object AddDoubles extends Addable[Double, Double, Double] { def add(l: Double, r: Double) = l + r }
  implicit def AddNumeric[A : Numeric]: Addable[A, A, A] = 
    new Addable[A, A, A] { def add(l: A, r: A) = implicitly[Numeric[A]].plus(l, r) }
  implicit val AddStrings: Addable[String, String, String] = 
    new Addable[String, String, String] { def add(l: String, r: String) = l + r }
  implicit def AddList[A]: Addable[List[A], List[A], List[A]] = 
    new Addable[List[A], List[A], List[A]] { def add(l: List[A], r: List[A]) = l ::: r }
  implicit val AddPeriod: Addable[Period, Period, Period] = 
    new Addable[Period, Period, Period] { def add(l: Period, r: Period) = l plus r }
  implicit val AddLocalDatePeriod: Addable[LocalDate, Period, LocalDate] = 
    new Addable[LocalDate, Period, LocalDate] { def add(l: LocalDate, r: Period) = l plus r }
  implicit val AddLocalDateTimePeriod: Addable[LocalDateTime, Period, LocalDateTime] = 
    new Addable[LocalDateTime, Period, LocalDateTime] { def add(l: LocalDateTime, r: Period) = l plus r }
  implicit val AddLocalTimePeriod: Addable[LocalTime, Period, LocalTime] = 
    new Addable[LocalTime, Period, LocalTime] { def add(l: LocalTime, r: Period) = l plus r }
  
  implicit def SubtractNumeric[A : Numeric]: Subtractable[A, A, A] = 
    new Subtractable[A, A, A] { def subtract(l: A, r: A) = implicitly[Numeric[A]].minus(l, r) }
  implicit val SubtractPeriod: Subtractable[Period, Period, Period] = 
    new Subtractable[Period, Period, Period] { def subtract(l: Period, r: Period) = l minus r }
  implicit val SubtractLocalDatePeriod: Subtractable[LocalDate, Period, LocalDate] = 
    new Subtractable[LocalDate, Period, LocalDate] { def subtract(l: LocalDate, r: Period) = l minus r }
  implicit val SubtractLocalDateTimePeriod: Subtractable[LocalDateTime, Period, LocalDateTime] = 
    new Subtractable[LocalDateTime, Period, LocalDateTime] { def subtract(l: LocalDateTime, r: Period) = l minus r }
  implicit val SubtractLocalTimePeriod: Subtractable[LocalTime, Period, LocalTime] = 
    new Subtractable[LocalTime, Period, LocalTime] { def subtract(l: LocalTime, r: Period) = l minus r }
  // FIXED (I think) conflicting implicits :'(
  implicit val SubtractLocalDate: Subtractable[LocalDate, LocalDate, Period] = 
    new Subtractable[LocalDate, LocalDate, Period] { def subtract(l: LocalDate, r: LocalDate) = new Period(l, r) }
  implicit val SubtractLocalDateTime: Subtractable[LocalDateTime, LocalDateTime, Period] = 
    new Subtractable[LocalDateTime, LocalDateTime, Period] { def subtract(l: LocalDateTime, r: LocalDateTime) = new Period(l, r) }
  implicit val SubtractLocalTime: Subtractable[LocalTime, LocalTime, Period] = 
    new Subtractable[LocalTime, LocalTime, Period] { def subtract(l: LocalTime, r: LocalTime) = new Period(l, r) }
  
  
  implicit def MultiplyNumeric[A : Numeric]: Multipliable[A, A, A] = 
    new Multipliable[A, A, A] { def multiply(l: A, r: A) = implicitly[Numeric[A]].times(l, r) }
  
  implicit def DivideFractional[A : Fractional]: Divisible[A, A, A] = 
    new Divisible[A, A, A] { def divide(l: A, r: A) = implicitly[Fractional[A]].div(l, r) }
  
  implicit def AbsNumeric[A : Numeric]: Absable[A, A] = 
    new Absable[A, A] { def absoluteValue(in: A) = implicitly[Numeric[A]].abs(in) }
  
  implicit def ListContains[A]: Containable[A, List[A]] = 
    new Containable[A, List[A]] { def isContainedIn(l: A, r: List[A]) = r contains l }
  
  implicit def EqualsAny[A]: Equatable[A, A] = 
    new Equatable[A, A] { def areEqual(l: A, r: A) = r == l }
  
  implicit def CompareOrdering[A : Ordering]: Comparable[A, A] = 
    new Comparable[A, A] { def greaterThan(l: A, r: A) = implicitly[Ordering[A]].gt(l, r) }
  //Provide an Ordering for LocalDate, LocalTime and LocalDateTime
  implicit def BaseLocalHasOrdering[A <: BaseLocal]: Ordering[A] = 
    new Ordering[A] { def compare(x: A, y: A) = x.compareTo(y) }
}
