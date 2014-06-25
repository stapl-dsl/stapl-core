package stapl.core

package object templates {
  
  /**
   * Actually, this is DenyIffNot
   */
  def OnlyPermitIff(id: String)(target: Expression, condition: Expression): PolicySet = 
    PolicySet(id) := when (target) apply PermitOverrides to (
        Policy("OnlyPermitIff-condition") := permit iff (condition),
        Policy("OnlyPermitIff-deny") := deny
    )
  
  /**
   * Actually, this is PermitIffNot
   */
  def OnlyDenyIff(id: String)(target: Expression, condition: Expression): PolicySet = 
    PolicySet(id) := when (target) apply DenyOverrides to (
        Policy("OnlyDenyIff-condition") := deny iff (condition),
        Policy("OnlyDenyIff-permit") := permit
    )
  
  def defaultPermit(id: String): Policy = 
    new Policy(id)(
	  target = AlwaysTrue,
	  effect = Permit,
	  condition = AlwaysTrue
	)

  def defaultDeny(id: String): Policy = 
    new Policy(id)(
	  target = AlwaysTrue,
	  effect = Deny,
	  condition = AlwaysTrue
	)

}