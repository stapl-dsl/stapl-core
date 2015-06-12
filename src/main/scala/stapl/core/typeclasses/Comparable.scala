package stapl.core.typeclasses

trait Comparable[L,R] {
  def greaterThan(l: L, r: R): Boolean
}