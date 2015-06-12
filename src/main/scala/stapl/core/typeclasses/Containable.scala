package stapl.core.typeclasses

trait Containable[L,R] {
  def isContainedIn(l: L, r: R): Boolean
}