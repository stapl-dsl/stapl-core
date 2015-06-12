package stapl.core.typeclasses

trait Multipliable[L,R,Out] {
  def multiply(l: L, r: R): Out
}