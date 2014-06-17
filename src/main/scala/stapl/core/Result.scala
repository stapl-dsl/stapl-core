package stapl.core

sealed trait Result

sealed trait Effect extends Result {
  def reverse(): Effect = this match {
    case Permit => Deny
    case Deny => Permit
  }
}

case object Permit extends Effect
  
case object Deny extends Effect

case object NotApplicable extends Result