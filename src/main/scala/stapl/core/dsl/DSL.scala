package stapl.core.dsl

import stapl.core._

/**
 * **************************************
 * The more natural DSL for policies and policy sets
 *
 * Examples for policies:
 * 	Policy("policy1") := when ("role" in subject.roles) deny iff (subject.allowed === false)
 *  Policy("policy2") := deny iff (subject.allowed === false)
 *  Policy("policy3") := when ("role" in subject.roles) deny
 *  Policy("policy4") := deny
 *
 * Examples for policy sets:
 * 	TODO
 *
 * FIXME the "policy" in this line should not be possible:
 * 	Policy("view document") := when (action.id === "view" & resource.type_ === "document") permit
 *  ====== Why not? ====== => because this is a policy and the keyword "permit" should not be in here
 *
 * TODO does a rule need a target?
 */
class OnlyIdRule(private val id: String) {

  def :=(t: EffectConditionAndObligationActions): Rule =
    new Rule(id)(t.effect, t.condition, List(t.obligationActions: _*))

  def :=(t: EffectAndCondition): Rule =
    new Rule(id)(t.effect, t.condition)

  def :=(t: EffectAndObligationActions): Rule =
    new Rule(id)(t.effect, LiteralExpression(true), List(t.obligationActions: _*))

  def :=(effectKeyword: EffectKeyword): Rule = effectKeyword match {
    case `deny` => new Rule(id)(Deny)
    case `permit` => new Rule(id)(Permit)
  }

}

class OnlyIdPolicy(private val id: String) {

  def :=(t: TargetPCASubpoliciesAndObligations): Policy =
    new Policy(id)(t.target, t.pca, t.subpolicies, t.obligations)

  def :=(t: TargetPCAAndSubpolicies): Policy =
    new Policy(id)(t.target, t.pca, List(t.subpolicies: _*))
}

class ObligationActionWithOn(val obligationAction: ObligationAction) {

  def on(effect: Effect): Obligation =
    new Obligation(obligationAction, effect)
}

class EffectAndCondition(val effect: Effect, val condition: Expression) {

  def performing(obligationActions: ObligationAction*): EffectConditionAndObligationActions =
    new EffectConditionAndObligationActions(effect, condition, obligationActions: _*)
}

class EffectConditionAndObligationActions(
  val effect: Effect, val condition: Expression, val obligationActions: ObligationAction*)

class EffectAndObligationActions(
  val effect: Effect, val obligationActions: ObligationAction*)

class EffectKeyword // FIXME this cannot be the best way to do this...
case object deny extends EffectKeyword {
  /**
   * Needed if no target is given
   */
  def iff(condition: Expression): EffectAndCondition =
    new EffectAndCondition(Deny, condition)

  def performing(obligationActions: ObligationAction*): EffectAndObligationActions =
    new EffectAndObligationActions(Deny, obligationActions: _*)
}
case object permit extends EffectKeyword {
  /**
   * Needed if no target is given
   */
  def iff(condition: Expression): EffectAndCondition =
    new EffectAndCondition(Permit, condition)

  def performing(obligationActions: ObligationAction*): EffectAndObligationActions =
    new EffectAndObligationActions(Permit, obligationActions: _*)
}

class TargetPCAAndSubpolicies(val target: Expression, val pca: CombinationAlgorithm, val subpolicies: AbstractPolicy*) {

  def performing(obligations: Obligation*): TargetPCASubpoliciesAndObligations =
    new TargetPCASubpoliciesAndObligations(target, pca, List(subpolicies: _*), List(obligations: _*))
}

class TargetPCASubpoliciesAndObligations(val target: Expression, val pca: CombinationAlgorithm,
  val subpolicies: List[AbstractPolicy], val obligations: List[Obligation])

class TargetAndPCA(val target: Expression, val pca: CombinationAlgorithm) {

  def to(subpolicies: AbstractPolicy*): TargetPCAAndSubpolicies =
    new TargetPCAAndSubpolicies(target, pca, subpolicies: _*)
}

class OnlyTarget(val target: Expression) {

  def apply(pca: CombinationAlgorithm): TargetAndPCA =
    new TargetAndPCA(target, pca)

}
object when {
  def apply(target: Expression = LiteralExpression(true)): OnlyTarget =
    new OnlyTarget(target)
}
object apply {

  /**
   * If no target is given for a policy set
   */
  def apply(pca: CombinationAlgorithm): TargetAndPCA =
    new TargetAndPCA(LiteralExpression(true), pca)

  def PermitOverrides(subpolicies: OnlySubpolicies): TargetPCAAndSubpolicies =
    new TargetPCAAndSubpolicies(LiteralExpression(true), stapl.core.PermitOverrides, subpolicies.subpolicies: _*)

  def DenyOverrides(subpolicies: OnlySubpolicies): TargetPCAAndSubpolicies =
    new TargetPCAAndSubpolicies(LiteralExpression(true), stapl.core.DenyOverrides, subpolicies.subpolicies: _*)

  def FirstApplicable(subpolicies: OnlySubpolicies): TargetPCAAndSubpolicies =
    new TargetPCAAndSubpolicies(LiteralExpression(true), stapl.core.FirstApplicable, subpolicies.subpolicies: _*)
}
class OnlySubpolicies(val subpolicies: AbstractPolicy*)
object to {

  def apply(subpolicies: AbstractPolicy*): OnlySubpolicies =
    new OnlySubpolicies(subpolicies: _*)
}
object iff {
  /**
   * Just to add the keyword "iff"
   */
  def apply(condition: Expression): Expression =
    condition
}
object Rule { // not really a companion object of Rule, but the start of the natural DSL for policies
  def apply(id: String) =
    new OnlyIdRule(id)
}
object Policy {
  def apply(id: String) =
    new OnlyIdPolicy(id)
}


object log {
  def apply(msg: Value[String]) = new LogObligationAction(msg)
}
object mail {
  def apply(to: String, msg: String) = new MailObligationAction(to, msg)
}
/**
 * Updating attribute values
 */
object update {
  def apply[T](attribute: Attribute[T], value: Value[T]) =
    new ChangeAttributeObligationAction(attribute, value, Update)
}

/**
 * Appending to attribute values
 */
object append {
  def apply[T](attribute: Attribute[T], value: Value[T]) =
    new ChangeAttributeObligationAction(attribute, value, Append)
}