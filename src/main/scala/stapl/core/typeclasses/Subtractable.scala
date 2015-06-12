package stapl.core.typeclasses

trait Subtractable[L,R,Out] {
  def subtract(l: L, r: R): Out 
}