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
  implicit class Plus[L](l: L) {
    def +[R, Out](r: Value[R])(implicit ev: Addable[L, R, Out]): Value[Out] = BinaryOp("+", Value(l), r, ev.add _)
  }
  implicit class PlusValue[L](l: Value[L]) {
    def +[R, Out](r: Value[R])(implicit ev: Addable[L, R, Out]): Value[Out] = BinaryOp("+", l, r, ev.add _)
  }
  
  implicit class Minus[L](l: L) {
    def -[R, Out](r: Value[R])(implicit ev: Subtractable[L, R, Out]): Value[Out] = BinaryOp("-", Value(l), r, ev.subtract _)
  }
  implicit class MinusValue[L](l: Value[L]) {
    def -[R, Out](r: Value[R])(implicit ev: Subtractable[L, R, Out]): Value[Out] = BinaryOp("-", l, r, ev.subtract _)
  }
  
  implicit class Times[L](l: L) {
    def *[R, Out](r: Value[R])(implicit ev: Multipliable[L, R, Out]): Value[Out] = BinaryOp("*", Value(l), r, ev.multiply _)
  }
  implicit class TimesValue[L](l: Value[L]) {
    def *[R, Out](r: Value[R])(implicit ev: Multipliable[L, R, Out]): Value[Out] = BinaryOp("*", l, r , ev.multiply _)
  }
  
  implicit class Divide[L](l: L) {
    def /[R, Out](r: Value[R])(implicit ev: Divisible[L, R, Out]): Value[Out] = BinaryOp("/", Value(l), r, ev.divide _)
  }
  implicit class DivideValue[L](l: Value[L]) {
    def /[R, Out](r: Value[R])(implicit ev: Divisible[L, R, Out]): Value[Out] = BinaryOp("/", l, r, ev.divide _)
  }
  
  implicit class Contains[L](l: L) {
    def in[R](r: Value[R])(implicit ev: Containable[L, R]): Value[Boolean] = BinaryOp("in", Value(l), r, ev.isContainedIn _)
  }
  implicit class ContainsValue[L, R](l: Value[L]) {
    def in[R](r: Value[R])(implicit ev: Containable[L, R]): Value[Boolean] = BinaryOp("in", l, r, ev.isContainedIn _)
  }
  
  implicit class IsEqual[L](l: L) {
    def ===[R](r: Value[R])(implicit ev: Equatable[L, R]): Value[Boolean] = BinaryOp("===", Value(l), r, ev.areEqual _)
  }
  implicit class IsEqualValue[L, R](l: Value[L]) {
    def ===[R](r: Value[R])(implicit ev: Equatable[L, R]): Value[Boolean] = BinaryOp("===", l, r , ev.areEqual _)
  }
  
  implicit class IsGreaterThan[L](l: L) {
    def >[R](r: Value[R])(implicit ev: Comparable[L, R]): Value[Boolean] = BinaryOp(">", Value(l), r, ev.greaterThan _)
    @inline def gt[R](r: Value[R])(implicit ev: Comparable[L, R]): Value[Boolean] = >(r)
  }
  implicit class IsGreaterThanValue[L](l: Value[L]) {
    def >[R](r: Value[R])(implicit ev: Comparable[L, R]): Value[Boolean] = BinaryOp(">", l, r, ev.greaterThan _)
    @inline def gt[R](r: Value[R])(implicit ev: Comparable[L, R]): Value[Boolean] = >(r)
  }
  
  implicit class ExtCompare[L, R](l: L) {
    private lazy val lt_function = (c: Comparable[L, R], e: Equatable[L, R]) => (x: L, y: R) => { !(c.greaterThan(x,y) || e.areEqual(x, y))}
    private lazy val lteq_function = (c: Comparable[L, R], e: Equatable[L, R]) => (x: L, y: R) => { !c.greaterThan(x,y) }
    private lazy val gteq_function = (c: Comparable[L, R], e: Equatable[L, R]) => (x: L, y: R) => { c.greaterThan(x,y) || e.areEqual(x, y) }
    
    def <(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = BinaryOp("<", Value(l), r, lt_function(c,e))
    @inline def lt(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = <(r)
    
    def <=(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = BinaryOp("<=", Value(l), r, lteq_function(c,e))
    @inline def lteq(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = <=(r)
    
    def >=(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = BinaryOp(">=", Value(l), r, gteq_function(c,e))
    @inline def gteq(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = >=(r)
  }
  implicit class ExtCompareValue[L, R](l: Value[L]) {
    private lazy val lt_function = (c: Comparable[L, R], e: Equatable[L, R]) => (x: L, y: R) => { !(c.greaterThan(x,y) || e.areEqual(x, y))}
    private lazy val lteq_function = (c: Comparable[L, R], e: Equatable[L, R]) => (x: L, y: R) => { !c.greaterThan(x,y) }
    private lazy val gteq_function = (c: Comparable[L, R], e: Equatable[L, R]) => (x: L, y: R) => { c.greaterThan(x,y) || e.areEqual(x, y) }
    
    def <(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = BinaryOp("<", l, r , lt_function(c,e))
    @inline def lt(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = <(r)
    
    def <=(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = BinaryOp("<=", l, r, lteq_function(c,e))
    @inline def lteq(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = <=(r)
    
    def >=(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = BinaryOp(">=",l , r, gteq_function(c,e))
    @inline def gteq(r: Value[R])(implicit c: Comparable[L, R], e: Equatable[L, R]): Value[Boolean] = >=(r)
  }
  
  def abs[In, Out](in: Value[In])(implicit ev: Absable[In, Out]): Value[Out] = UnaryOp("abs", in, ev.absoluteValue _)
}
