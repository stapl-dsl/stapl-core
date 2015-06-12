package stapl.core.typeclasses

trait Absable[In,Out] {
  def absoluteValue(in: In): Out
}