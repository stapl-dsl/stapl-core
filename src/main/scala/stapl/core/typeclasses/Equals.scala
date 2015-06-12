package stapl.core.typeclasses

trait Equals[L,R] {
  def areEqual(l: L, r: R): Boolean
}