package stapl.core

/**
 * A simple base class to extend from and start writing policies. 
 * Extending this class imports subject, resource, action and env into 
 * your scope, avoiding writing this boiler plate code yourself.
 * 
 * Example: see staple.core.examples.PolicyFromReadMe 
 */
trait BasicPolicy {
  
  // repeat the definitions from stapl.core so that we do not work 
  // on a single subject/resource/action/environment object
  val (subject, action, resource, environment) = BasicPolicy.containers

}


object BasicPolicy {
  
  /**
   * USAGE: val (subject, action, resource, environment) = BasicPolicy.containers
   */
  def containers = {
    val subject = new SubjectAttributeContainer
    subject.id = SimpleAttribute(String) 
    val resource = new ResourceAttributeContainer
    resource.id = SimpleAttribute(String)
    val action = new ActionAttributeContainer
    action.id = SimpleAttribute(String) 
    val environment = new EnvironmentAttributeContainer
    
    (subject, action, resource, environment)
  }
}