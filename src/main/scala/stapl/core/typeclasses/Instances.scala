package stapl.core.typeclasses

trait Instances {
  implicit object AddDoubles extends Addable[Double, Double, Double] { def add(l: Double, r: Double) = l + r }
  implicit object AddStrings extends Addable[String, String, String] { def add(l: String, r: String) = l + r }
  implicit def AddList[A] = new Addable[List[A], List[A], List[A]] { def add(l: List[A], r: List[A]) = l ::: r }
  
  implicit object SubtractDoubles extends Subtractable[Double, Double, Double] { def subtract(l: Double, r: Double) = l - r }
  
  implicit object MultiplyDoubles extends Multipliable[Double, Double, Double] { def multiply(l: Double, r: Double) = l * r }
  
  implicit object DivideDoubles extends Divisible[Double, Double, Double] { def divide(l: Double, r: Double) = l / r }
  
  implicit object AbsDoubles extends Absable[Double, Double] { def absoluteValue(in: Double) = Math.abs(in) }
  
  implicit def ListContains[A] = new Containable[A, List[A], Boolean] { def isContainedIn(l: A, r: List[A]) = r contains l }
}