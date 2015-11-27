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

import stapl.core.typeclasses._
import stapl.core.Value
import stapl.core.BinaryOp
import stapl.core.UnaryOp
import scala.annotation.implicitNotFound

trait Syntax {
  implicit class Plus[A, L <% Value[A]](l: L) {
    def +[B, Out](r: Value[B])(implicit ev: Addable[A, B, Out]): Value[Out] = BinaryOp("+", l, r, ev.add _)
  }
  
  implicit class Minus[A, L <% Value[A]](l: L) {
    def -[B, Out](r: Value[B])(implicit ev: Subtractable[A, B, Out]): Value[Out] = BinaryOp("-", l, r, ev.subtract _)
  }
  
  implicit class Times[A, L <% Value[A]](l: L) {
    def *[B, Out](r: Value[B])(implicit ev: Multipliable[A, B, Out]): Value[Out] = BinaryOp("*", l, r, ev.multiply _)
  }
  
  implicit class Divide[A, L <% Value[A]](l: L) {
    def /[B, Out](r: Value[B])(implicit ev: Divisible[A, B, Out]): Value[Out] = BinaryOp("/", l, r, ev.divide _)
  }
  
  implicit class Contains[A, L <% Value[A]](l: L) {
    def in[B](r: Value[B])(implicit ev: Containable[A, B]): Value[Boolean] = BinaryOp("in", l, r, ev.isContainedIn _)
  }
  
  implicit class IsEqual[A, L <% Value[A]](l: L) {
    def ===[B](r: Value[B])(implicit ev: Equatable[A, B]): Value[Boolean] = BinaryOp("===", l, r, ev.areEqual _)
  }
  
  implicit class IsGreaterThan[A, L <% Value[A]](l: L) {
    def >[B](r: Value[B])(implicit ev: Comparable[A, B]): Value[Boolean] = BinaryOp(">", l, r, ev.greaterThan _)
    @inline def gt[B](r: Value[B])(implicit ev: Comparable[A, B]): Value[Boolean] = >(r)
  }
  
  implicit class ExtCompare[A, L <% Value[A]](l: L) {
    def <[B](r: Value[B])(implicit c: Comparable[A, B], e: Equatable[A, B]): Value[Boolean] = BinaryOp("<", l, r, (x: A, y: B) => { !(c.greaterThan(x,y) || e.areEqual(x, y))})
    @inline def lt[B](r: Value[B])(implicit c: Comparable[A, B], e: Equatable[A, B]): Value[Boolean] = <(r)
    
    def <=[B](r: Value[B])(implicit c: Comparable[A, B], e: Equatable[A, B]): Value[Boolean] = BinaryOp("<=", l, r, (x: A, y: B) => { !c.greaterThan(x,y) })
    @inline def lteq[B](r: Value[B])(implicit c: Comparable[A, B], e: Equatable[A, B]): Value[Boolean] = <=(r)
    
    def >=[B](r: Value[B])(implicit c: Comparable[A, B], e: Equatable[A, B]): Value[Boolean] = BinaryOp(">=", l, r, (x: A, y: B) => { c.greaterThan(x,y) || e.areEqual(x, y) })
    @inline def gteq[B](r: Value[B])(implicit c: Comparable[A, B], e: Equatable[A, B]): Value[Boolean] = >=(r)
  }
  
  def abs[A, Out](in: Value[A])(implicit ev: Absable[A, Out]): Value[Out] = UnaryOp("abs", in, ev.absoluteValue _)
}
