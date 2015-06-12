package stapl.core.typeclasses

trait Divisible[L,R,Out] {
  def divide(l: L, r: R): Out
}