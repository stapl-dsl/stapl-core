package stapl.core.typeclasses

trait Addable[L,R,Out] {
  def add(l: L, r: R): Out
}