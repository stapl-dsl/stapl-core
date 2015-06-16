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

package stapl.core.syntax

import stapl.core.typeclasses._
import stapl.core.Value
import stapl.core.BinaryOp
import stapl.core.UnaryOp
import stapl.core.UnaryOp

trait Syntax {
  implicit class Plus[L, R, Out](l: L) {
    def +(r: Value[R])(implicit ev: Addable[L, R, Out]): Value[Out] = BinaryOp("+", Value(l), r, ev.add _)
  }
  implicit class PlusValue[L, R, Out](l: Value[L]) {
    def +(r: Value[R])(implicit ev: Addable[L, R, Out]): Value[Out] = BinaryOp("+", l, r, ev.add _)
  }
  
  implicit class Minus[L, R, Out](l: L) {
    def -(r: Value[R])(implicit ev: Subtractable[L, R, Out]): Value[Out] = BinaryOp("-", Value(l), r, ev.subtract _)
  }
  implicit class MinusValue[L, R, Out](l: Value[L]) {
    def -(r: Value[R])(implicit ev: Subtractable[L, R, Out]): Value[Out] = BinaryOp("-", l, r, ev.subtract _)
  }
  
  implicit class Times[L, R, Out](l: L) {
    def *(r: Value[R])(implicit ev: Multipliable[L, R, Out]): Value[Out] = BinaryOp("*", Value(l), r, ev.multiply _)
  }
  implicit class TimesValue[L, R, Out](l: Value[L]) {
    def *(r: Value[R])(implicit ev: Multipliable[L, R, Out]): Value[Out] = BinaryOp("*", l, r , ev.multiply _)
  }
  
  implicit class Divide[L, R, Out](l: L) {
    def /(r: Value[R])(implicit ev: Divisible[L, R, Out]): Value[Out] = BinaryOp("/", Value(l), r, ev.divide _)
  }
  implicit class DivideValue[L, R, Out](l: Value[L]) {
    def /(r: Value[R])(implicit ev: Divisible[L, R, Out]): Value[Out] = BinaryOp("/", l, r, ev.divide _)
  }
  
  implicit class Contains[L, R](l: L) {
    def in(r: Value[R])(implicit ev: Containable[L, R]): Value[Boolean] = BinaryOp("in", Value(l), r, ev.isContainedIn _)
  }
  implicit class ContainsValue[L, R](l: Value[L]) {
    def in(r: Value[R])(implicit ev: Containable[L, R]): Value[Boolean] = BinaryOp("in", l, r, ev.isContainedIn _)
  }
  
  implicit class IsEqual[L, R](l: L) {
    def ===(r: Value[R])(implicit ev: Equals[L, R]): Value[Boolean] = BinaryOp("===", Value(l), r, ev.areEqual _)
  }
  implicit class IsEqualValue[L, R](l: Value[L]) {
    def ===(r: Value[R])(implicit ev: Equals[L, R]): Value[Boolean] = BinaryOp("===", l, r , ev.areEqual _)
  }
  
  implicit class IsGreaterThan[L, R](l: L) {
    def gt(r: Value[R])(implicit ev: Comparable[L, R]): Value[Boolean] = BinaryOp("gt", Value(l), r, ev.greaterThan _)
  }
  implicit class IsGreaterThanValue[L, R](l: Value[L]) {
    def gt(r: Value[R])(implicit ev: Comparable[L, R]): Value[Boolean] = BinaryOp("gt", l, r, ev.greaterThan _)
  }
  
  implicit class ExtCompare[L, R](l: L) {
    private val lt_function = (c: Comparable[L, R], e: Equals[L, R]) => (x: L, y: R) => { !(c.greaterThan(x,y) || e.areEqual(x, y))}
    private val lteq_function = (c: Comparable[L, R], e: Equals[L, R]) => (x: L, y: R) => { !c.greaterThan(x,y) }
    private val gteq_function = (c: Comparable[L, R], e: Equals[L, R]) => (x: L, y: R) => { c.greaterThan(x,y) || e.areEqual(x, y) }
    
    def lt(r: Value[R])(implicit c: Comparable[L, R], e: Equals[L, R]): Value[Boolean] = BinaryOp("lt", Value(l), r, lt_function(c,e))
    
    def lteq(r: Value[R])(implicit c: Comparable[L, R], e: Equals[L, R]): Value[Boolean] = BinaryOp("lteq", Value(l), r, lteq_function(c,e))
    
    def gteq(r: Value[R])(implicit c: Comparable[L, R], e: Equals[L, R]): Value[Boolean] = BinaryOp("gteq", Value(l), r, gteq_function(c,e))
  }
  implicit class ExtCompareValue[L, R](l: Value[L]) {
    private val lt_function = (c: Comparable[L, R], e: Equals[L, R]) => (x: L, y: R) => { !(c.greaterThan(x,y) || e.areEqual(x, y))}
    private val lteq_function = (c: Comparable[L, R], e: Equals[L, R]) => (x: L, y: R) => { !c.greaterThan(x,y) }
    private val gteq_function = (c: Comparable[L, R], e: Equals[L, R]) => (x: L, y: R) => { c.greaterThan(x,y) || e.areEqual(x, y) }
    
    def lt(r: Value[R])(implicit c: Comparable[L, R], e: Equals[L, R]): Value[Boolean] = BinaryOp("lt", l, r , lt_function(c,e))
    
    def lteq(r: Value[R])(implicit c: Comparable[L, R], e: Equals[L, R]): Value[Boolean] = BinaryOp("lteq", l, r, lteq_function(c,e))
    
    def gteq(r: Value[R])(implicit c: Comparable[L, R], e: Equals[L, R]): Value[Boolean] = BinaryOp("gteq",l , r, gteq_function(c,e))
  }
  
  def abs[In, Out](in: Value[In])(implicit ev: Absable[In, Out]): Value[Out] = UnaryOp("abs", in, ev.absoluteValue _)
}
