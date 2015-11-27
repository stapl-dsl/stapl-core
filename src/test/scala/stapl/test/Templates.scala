package stapl.test

trait Templates {
  import stapl.core.dsl._
  import templating._
  
  
  def OnlyPermitIff(id: String)(target: Expression)(condition: Expression) =
    Policy(id) := when(target) apply PermitOverrides to (
      Rule("OnlyPermitIff-condition") := permit iff (condition),
      Rule("OnlyPermitIff-deny") := deny)

  /**
   * TODO Actually, this is PermitIffNot
   */
  def OnlyDenyIff(id: String)(target: Expression)(condition: Expression) =
    Policy(id) := when(target) apply DenyOverrides to (
      Rule("OnlyDenyIff-condition") := deny iff (condition),
      Rule("OnlyDenyIff-permit") := permit)


  /**
   * Always returns Permit on every request.
   */
  def defaultPermit(id: String) =
    Rule(id) := permit

  /**
   * Always returns Deny on every request.
   */
  def defaultDeny(id: String) =
    Rule(id) := deny
}