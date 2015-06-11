package stapl.core

/**
 * A simple base class to extend from and start writing policies. 
 * Extending this class imports subject, resource, action and env into 
 * your scope, avoiding writing this boiler plate code yourself.
 * 
 * Example: see staple.core.examples.PolicyFromReadMe 
 */
trait BasicPolicy {
  
  trait BasicSubject extends Subject {
    val id = Attribute[String]("id")
  }
  
  trait BasicResource extends Resource {
    val id = Attribute[String]("id")
  }
  
  trait BasicAction extends Action {
    val id = Attribute[String]("id")
  }

}