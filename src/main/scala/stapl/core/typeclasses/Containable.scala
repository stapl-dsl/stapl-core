package stapl.core.typeclasses

trait Containable[L,R,Out] {
  def isContainedIn(l: L, r: R): Out
}