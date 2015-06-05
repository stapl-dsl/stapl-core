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
   * TODO put macro in seperate project!
   */
  def containers = {
    /*val subject = new Subject {
      val id = SimpleAttribute(String) 
    }
    val resource = new Resource {
      val id = SimpleAttribute(String) 
    }
    val action = new Action {
      val id = SimpleAttribute(String) 
    }
    val environment = new Environment{}
    
    (subject, action, resource, environment)*/
    (null, null, null, null)
  }
}