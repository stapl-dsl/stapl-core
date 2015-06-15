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

package stapl.core.typeclasses

import org.joda.time.Period
import org.joda.time.LocalDate
import org.joda.time.LocalDateTime
import org.joda.time.LocalTime
import org.joda.time.base.BaseLocal

trait Instances {
  //implicit object AddDoubles extends Addable[Double, Double, Double] { def add(l: Double, r: Double) = l + r }
  implicit def AddNumeric[A : Numeric] = new Addable[A, A, A] { def add(l: A, r: A) = implicitly[Numeric[A]].plus(l, r) }
  implicit object AddStrings extends Addable[String, String, String] { def add(l: String, r: String) = l + r }
  implicit def AddList[A] = new Addable[List[A], List[A], List[A]] { def add(l: List[A], r: List[A]) = l ::: r }
  implicit object AddPeriod extends Addable[Period, Period, Period] { def add(l: Period, r: Period) = l plus r }
  implicit object AddLocalDatePeriod extends Addable[LocalDate, Period, LocalDate] { def add(l: LocalDate, r: Period) = l plus r }
  implicit object AddLocalDateTimePeriod extends Addable[LocalDateTime, Period, LocalDateTime] { def add(l: LocalDateTime, r: Period) = l plus r }
  implicit object AddLocalTimePeriod extends Addable[LocalTime, Period, LocalTime] { def add(l: LocalTime, r: Period) = l plus r }
  
  implicit def SubtractNumeric[A : Numeric] = new Subtractable[A, A, A] { def subtract(l: A, r: A) = implicitly[Numeric[A]].minus(l, r) }
  implicit object SubtractPeriod extends Subtractable[Period, Period, Period] { def subtract(l: Period, r: Period) = l minus r }
  implicit object SubtractLocalDatePeriod extends Subtractable[LocalDate, Period, LocalDate] { def subtract(l: LocalDate, r: Period) = l minus r }
  implicit object SubtractLocalDateTimePeriod extends Subtractable[LocalDateTime, Period, LocalDateTime] { def subtract(l: LocalDateTime, r: Period) = l minus r }
  implicit object SubtractLocalTimePeriod extends Subtractable[LocalTime, Period, LocalTime] { def subtract(l: LocalTime, r: Period) = l minus r }
  // FIXED (I think) conflicting implicits :'(
  implicit object SubtractLocalDate extends Subtractable[LocalDate, LocalDate, Period] { def subtract(l: LocalDate, r: LocalDate) = new Period(l, r) }
  implicit object SubtractLocalDateTime extends Subtractable[LocalDateTime, LocalDateTime, Period] { def subtract(l: LocalDateTime, r: LocalDateTime) = new Period(l, r) }
  implicit object SubtractLocalTime extends Subtractable[LocalTime, LocalTime, Period] { def subtract(l: LocalTime, r: LocalTime) = new Period(l, r) }
  
  
  implicit def MultiplyNumeric[A : Numeric] = new Multipliable[A, A, A] { def multiply(l: A, r: A) = implicitly[Numeric[A]].times(l, r) }
  
  implicit def DivideFractional[A : Fractional] = new Divisible[A, A, A] { def divide(l: A, r: A) = implicitly[Fractional[A]].div(l, r) }
  
  implicit def AbsNumeric[A : Numeric] = new Absable[A, A] { def absoluteValue(in: A) = implicitly[Numeric[A]].abs(in) }
  
  implicit def ListContains[A] = new Containable[A, List[A]] { def isContainedIn(l: A, r: List[A]) = r contains l }
  
  implicit def EqualsAny[A] = new Equals[A, A] { def areEqual(l: A, r: A) = r == l }
  
  implicit def CompareOrdering[A : Ordering] = new Comparable[A, A] { def greaterThan(l: A, r: A) = implicitly[Ordering[A]].gt(l, r) }
  //Provide an Ordering for LocalDate, LocalTime and LocalDateTime
  implicit def BaseLocalHasOrdering[A <: BaseLocal]: Ordering[A] = new Ordering[A] {
    def compare(x: A, y: A) = x.compareTo(y)
  }
}
