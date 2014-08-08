package stapl.core

/**
 * A simple base class to extend from and start writing policies. 
 * Extending this class imports subject, resource, action and env into 
 * your scope, avoiding writing this boiler plat code yourself.
 * 
 * Example: see staple.core.examples.PolicyFromReadMe 
 */
trait BasicPolicy extends DelayedInit {

  val subject = stapl.core.subject // FIXME do we work on the single subject object here? we need a local copy of some sort
  val resource = stapl.core.resource
  val action = stapl.core.action
  val env = stapl.core.environment
  
  override def delayedInit(body: => Unit): Unit = {
    val code = (() => body)
    code()
  }

}