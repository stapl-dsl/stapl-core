package stapl.core.syntax

import stapl.core.typeclasses._
import stapl.core.Value
import stapl.core.BinaryOp
import stapl.core.UnaryOp

trait Syntax {
  implicit class Plus[L, R, Out](l: L)(implicit ev: Addable[L, R, Out]) {
    def +(r: R): Value[Out] = BinaryOp("+", Value(l), Value(r) , ev.add _)
    def +(r: Value[R]): Value[Out] = BinaryOp("+", Value(l), r , ev.add _)
  }
  implicit class PlusValue[L, R, Out](l: Value[L])(implicit ev: Addable[L, R, Out]) {
    def +(r: R): Value[Out] = BinaryOp("+", l, Value(r) , ev.add _)
    def +(r: Value[R]): Value[Out] = BinaryOp("+", l, r , ev.add _)
  }
  
  implicit class Minus[L, R, Out](l: L)(implicit ev: Subtractable[L, R, Out]) {
    def -(r: R): Value[Out] = BinaryOp("-", Value(l), Value(r) , ev.subtract _)
    def -(r: Value[R]): Value[Out] = BinaryOp("-", Value(l), r , ev.subtract _)
  }
  implicit class MinusValue[L, R, Out](l: Value[L])(implicit ev: Subtractable[L, R, Out]) {
    def -(r: R): Value[Out] = BinaryOp("-", l, Value(r) , ev.subtract _)
    def -(r: Value[R]): Value[Out] = BinaryOp("-", l, r , ev.subtract _)
  }
  
  implicit class Times[L, R, Out](l: L)(implicit ev: Multipliable[L, R, Out]) {
    def *(r: R): Value[Out] = BinaryOp("*", Value(l), Value(r) , ev.multiply _)
    def *(r: Value[R]): Value[Out] = BinaryOp("*", Value(l), r , ev.multiply _)
  }
  implicit class TimesValue[L, R, Out](l: Value[L])(implicit ev: Multipliable[L, R, Out]) {
    def *(r: R): Value[Out] = BinaryOp("*", l, Value(r) , ev.multiply _)
    def *(r: Value[R]): Value[Out] = BinaryOp("*", l, r , ev.multiply _)
  }
  
  implicit class Divide[L, R, Out](l: L)(implicit ev: Divisible[L, R, Out]) {
    def /(r: R): Value[Out] = BinaryOp("/", Value(l), Value(r) , ev.divide _)
    def /(r: Value[R]): Value[Out] = BinaryOp("/", Value(l), r , ev.divide _)
  }
  implicit class DivideValue[L, R, Out](l: Value[L])(implicit ev: Divisible[L, R, Out]) {
    def /(r: R): Value[Out] = BinaryOp("/", l, Value(r) , ev.divide _)
    def /(r: Value[R]): Value[Out] = BinaryOp("/", l, r , ev.divide _)
  }
  
  implicit class Contains[L, R, Out](l: L)(implicit ev: Containable[L, R, Out]) {
    def in(r: R): Value[Out] = BinaryOp("in", Value(l), Value(r) , ev.isContainedIn _)
    def in(r: Value[R]): Value[Out] = BinaryOp("in", Value(l), r , ev.isContainedIn _)
  }
  implicit class ContainsValue[L, R, Out](l: Value[L])(implicit ev: Containable[L, R, Out]) {
    def in(r: R): Value[Out] = BinaryOp("in", l, Value(r) , ev.isContainedIn _)
    def in(r: Value[R]): Value[Out] = BinaryOp("in", l, r , ev.isContainedIn _)
  }
  
  implicit class Abs[In, Out](in: In)(implicit ev: Absable[In, Out]) {
    def abs: Value[Out] = UnaryOp("abs", Value(in), ev.absoluteValue _)
  }
  implicit class AbsValue[In, Out](in: Value[In])(implicit ev: Absable[In, Out]) {
    def abs: Value[Out] = UnaryOp("abs", in, ev.absoluteValue _)
  }
}