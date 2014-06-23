package stapl.core

package object templates {
  
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