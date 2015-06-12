package stapl.core.syntax

import stapl.core.typeclasses._
import stapl.core.Value
import stapl.core.BinaryOp
import stapl.core.UnaryOp

trait Syntax {
  implicit class Plus[L, R, Out](l: L)(implicit ev: Addable[L, R, Out]) {
    def +(r: R): Value[Out] = BinaryOp("+", Value(l), Value(r), ev.add _)
    def +(r: Value[R]): Value[Out] = BinaryOp("+", Value(l), r, ev.add _)
  }
  implicit class PlusValue[L, R, Out](l: Value[L])(implicit ev: Addable[L, R, Out]) {
    def +(r: R): Value[Out] = BinaryOp("+", l, Value(r), ev.add _)
    def +(r: Value[R]): Value[Out] = BinaryOp("+", l, r, ev.add _)
  }
  
  implicit class Minus[L, R, Out](l: L)(implicit ev: Subtractable[L, R, Out]) {
    def -(r: R): Value[Out] = BinaryOp("-", Value(l), Value(r), ev.subtract _)
    def -(r: Value[R]): Value[Out] = BinaryOp("-", Value(l), r, ev.subtract _)
  }
  implicit class MinusValue[L, R, Out](l: Value[L])(implicit ev: Subtractable[L, R, Out]) {
    def -(r: R): Value[Out] = BinaryOp("-", l, Value(r), ev.subtract _)
    def -(r: Value[R]): Value[Out] = BinaryOp("-", l, r, ev.subtract _)
  }
  
  implicit class Times[L, R, Out](l: L)(implicit ev: Multipliable[L, R, Out]) {
    def *(r: R): Value[Out] = BinaryOp("*", Value(l), Value(r), ev.multiply _)
    def *(r: Value[R]): Value[Out] = BinaryOp("*", Value(l), r, ev.multiply _)
  }
  implicit class TimesValue[L, R, Out](l: Value[L])(implicit ev: Multipliable[L, R, Out]) {
    def *(r: R): Value[Out] = BinaryOp("*", l, Value(r) , ev.multiply _)
    def *(r: Value[R]): Value[Out] = BinaryOp("*", l, r , ev.multiply _)
  }
  
  implicit class Divide[L, R, Out](l: L)(implicit ev: Divisible[L, R, Out]) {
    def /(r: R): Value[Out] = BinaryOp("/", Value(l), Value(r), ev.divide _)
    def /(r: Value[R]): Value[Out] = BinaryOp("/", Value(l), r, ev.divide _)
  }
  implicit class DivideValue[L, R, Out](l: Value[L])(implicit ev: Divisible[L, R, Out]) {
    def /(r: R): Value[Out] = BinaryOp("/", l, Value(r), ev.divide _)
    def /(r: Value[R]): Value[Out] = BinaryOp("/", l, r, ev.divide _)
  }
  
  implicit class Contains[L, R](l: L)(implicit ev: Containable[L, R]) {
    def in(r: R): Value[Boolean] = BinaryOp("in", Value(l), Value(r), ev.isContainedIn _)
    def in(r: Value[R]): Value[Boolean] = BinaryOp("in", Value(l), r, ev.isContainedIn _)
  }
  implicit class ContainsValue[L, R](l: Value[L])(implicit ev: Containable[L, R]) {
    def in(r: R): Value[Boolean] = BinaryOp("in", l, Value(r), ev.isContainedIn _)
    def in(r: Value[R]): Value[Boolean] = BinaryOp("in", l, r, ev.isContainedIn _)
  }
  
  implicit class IsEqual[L, R](l: L)(implicit ev: Equals[L, R]) {
    def ===(r: R): Value[Boolean] = BinaryOp("===", Value(l), Value(r), ev.areEqual _)
    def ===(r: Value[R]): Value[Boolean] = BinaryOp("===", Value(l), r, ev.areEqual _)
  }
  implicit class IsEqualValue[L, R](l: Value[L])(implicit ev: Equals[L, R]) {
    def ===(r: R): Value[Boolean] = BinaryOp("===", l, Value(r) , ev.areEqual _)
    def ===(r: Value[R]): Value[Boolean] = BinaryOp("===", l, r , ev.areEqual _)
  }
  
  implicit class IsGreaterThan[L, R](l: L)(implicit ev: Comparable[L, R]) {
    def gt(r: R): Value[Boolean] = BinaryOp("gt", Value(l), Value(r), ev.greaterThan _)
    def gt(r: Value[R]): Value[Boolean] = BinaryOp("gt", Value(l), r, ev.greaterThan _)
  }
  implicit class IsGreaterThanValue[L, R](l: Value[L])(implicit ev: Comparable[L, R]) {
    def gt(r: R): Value[Boolean] = BinaryOp("gt", l, Value(r), ev.greaterThan _)
    def gt(r: Value[R]): Value[Boolean] = BinaryOp("gt", l, r, ev.greaterThan _)
  }
  
  implicit class ExtCompare[L, R](l: L)(implicit c: Comparable[L, R], e: Equals[L, R]) {
    private val lt_function = (x: L, y: R) => { !(c.greaterThan(x,y) || e.areEqual(x, y))}
    private val lteq_function = (x: L, y: R) => { !c.greaterThan(x,y) }
    private val gteq_function = (x: L, y: R) => { c.greaterThan(x,y) || e.areEqual(x, y) }
    
    def lt(r: R): Value[Boolean] = BinaryOp("lt", Value(l), Value(r), lt_function)
    def lt(r: Value[R]): Value[Boolean] = BinaryOp("lt", Value(l), r, lt_function)
    
    def lteq(r: R): Value[Boolean] = BinaryOp("lteq", Value(l), Value(r), lteq_function)
    def lteq(r: Value[R]): Value[Boolean] = BinaryOp("lteq", Value(l), r, lteq_function)
    
    def gteq(r: R): Value[Boolean] = BinaryOp("gteq", Value(l), Value(r), gteq_function)
    def gteq(r: Value[R]): Value[Boolean] = BinaryOp("gteq", Value(l), r, gteq_function)
  }
  implicit class ExtCompareValue[L, R](l: Value[L])(implicit c: Comparable[L, R], e: Equals[L, R]) {
    private val lt_function = (x: L, y: R) => { !(c.greaterThan(x,y) || e.areEqual(x, y))}
    private val lteq_function = (x: L, y: R) => { !c.greaterThan(x,y) }
    private val gteq_function = (x: L, y: R) => { c.greaterThan(x,y) || e.areEqual(x, y) }
    
    def lt(r: R): Value[Boolean] = BinaryOp("lt", l, Value(r) , lt_function)
    def lt(r: Value[R]): Value[Boolean] = BinaryOp("lt", l, r , lt_function)
    
    def lteq(r: R): Value[Boolean] = BinaryOp("lteq", l, Value(r), lteq_function)
    def lteq(r: Value[R]): Value[Boolean] = BinaryOp("lteq", l, r, lteq_function)
    
    def gteq(r: R): Value[Boolean] = BinaryOp("gteq", l, Value(r), gteq_function)
    def gteq(r: Value[R]): Value[Boolean] = BinaryOp("gteq",l , r, gteq_function)
  }
  
  implicit class Abs[In, Out](in: In)(implicit ev: Absable[In, Out]) {
    def abs: Value[Out] = UnaryOp("abs", Value(in), ev.absoluteValue _)
  }
  implicit class AbsValue[In, Out](in: Value[In])(implicit ev: Absable[In, Out]) {
    def abs: Value[Out] = UnaryOp("abs", in, ev.absoluteValue _)
  }
}