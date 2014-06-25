package stapl.core

case class Result(val decision: Decision, val obligations: List[Obligation] = List.empty)

sealed trait Decision

sealed trait Effect extends Decision {
  def reverse(): Effect = this match {
    case Permit => Deny
    case Deny => Permit
  }
}

case object Permit extends Effect
  
case object Deny extends Effect

case object NotApplicable extends Decision