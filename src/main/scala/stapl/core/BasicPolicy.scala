package stapl.core

/**
 * A simple base class to extend from and start writing policies. 
 * Extending this class imports subject, resource, action and env into 
 * your scope, avoiding writing this boiler plat code yourself.
 * 
 * Example: see staple.core.examples.PolicyFromReadMe 
 */
trait BasicPolicy extends DelayedInit {
  
  // repeat the definitions from stapl.core so that we do not work 
  // on a single subject/resource/action/evironment object
  val subject = new SubjectAttributeContainer
  subject.id = SimpleAttribute(String) 
  val resource = new ResourceAttributeContainer
  resource.id = SimpleAttribute(String)
  val action = new ActionAttributeContainer
  action.id = SimpleAttribute(String) 
  val environment = new EnvironmentAttributeContainer
  
  override def delayedInit(body: => Unit): Unit = {
    val code = (() => body)
    code()
  }

}