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

trait Instances {
  //implicit object AddDoubles extends Addable[Double, Double, Double] { def add(l: Double, r: Double) = l + r }
  implicit def AddNumeric[A : Numeric] = new Addable[A, A, A] { def add(l: A, r: A) = implicitly[Numeric[A]].plus(l, r) }
  implicit object AddStrings extends Addable[String, String, String] { def add(l: String, r: String) = l + r }
  implicit def AddList[A] = new Addable[List[A], List[A], List[A]] { def add(l: List[A], r: List[A]) = l ::: r }
  
  implicit def SubtractNumeric[A : Numeric] = new Subtractable[A, A, A] { def subtract(l: A, r: A) = implicitly[Numeric[A]].minus(l, r) }
  
  implicit def MultiplyNumeric[A : Numeric] = new Multipliable[A, A, A] { def multiply(l: A, r: A) = implicitly[Numeric[A]].times(l, r) }
  
  implicit def DivideFractional[A : Fractional] = new Divisible[A, A, A] { def divide(l: A, r: A) = implicitly[Fractional[A]].div(l, r) }
  
  implicit def AbsNumeric[A : Numeric] = new Absable[A, A] { def absoluteValue(in: A) = implicitly[Numeric[A]].abs(in) }
  
  implicit def ListContains[A] = new Containable[A, List[A]] { def isContainedIn(l: A, r: List[A]) = r contains l }
  
  implicit def EqualsAny[A] = new Equals[A, A] { def areEqual(l: A, r: A) = r == l }
  
  implicit def CompareOrdering[A : Ordering] = new Comparable[A, A] { def greaterThan(l: A, r: A) = implicitly[Ordering[A]].gt(l, r) }
}
