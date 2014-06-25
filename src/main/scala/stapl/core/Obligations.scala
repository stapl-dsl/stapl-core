package stapl.core

/**
 * 
 */
case class Obligation(val action: ObligationAction, val fulfillOn: Effect)

/**
 * Class used for representing the action to be performed as specified in
 * an obligation.
 */
abstract class ObligationAction

case class LogObligationAction(val msg: Value) extends ObligationAction
object log {
  def apply(msg: Value) = new LogObligationAction(msg)
}

case class MailObligationAction(val to: String, val msg: String) extends ObligationAction
object mail {
  def apply(to: String, msg: String) = new MailObligationAction(to, msg)
}

case class UpdateAttributeObligationAction(val attribute: Attribute, val value: ConcreteValue) extends ObligationAction
object update {
  def apply(attribute: Attribute, value: ConcreteValue) =
    new UpdateAttributeObligationAction(attribute, value)
}