package stapl.core.parser

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import stapl.core.AbstractPolicy

/**
 * A class for parsing policies dynamically from strings.
 * This parser employs the Scala interpreter.
 */
class PolicyParser {
  val settings = new Settings
  settings.usejavacp.value = true
  settings.nowarnings.value = true
  val interpreter = new IMain(settings)
  interpreter.beQuietDuring({
    interpreter.interpret(
      "import stapl.core.{subject => _, resource => _, action => _, _}\n" +
      "import stapl.core.templates._")
  })
  
  /**
   * Add another import to the Scala interpreter employed by this parser.
   */
  def addImport(i: String) = {
    interpreter.beQuietDuring({
      interpreter.interpret(s"import $i")
    })
  }

  /**
   * Parse the given policy string and return the resulting policy.
   * 
   * @param		policyString: String
   * 			The string containing the STAPL policy. This policy should not
   *    		contain attribute definitions and should just contain the policy
   *      		specification, not an assignment to a val. 
   *        	For example: policyString = "Policy(...) := ..."
   * 
   * @throws 	RuntimeException	
   * 			When the parser did not success in getting 
   * 			an abstract policy from the given string.
   */
  def parse(policyString: String): AbstractPolicy = {
    val completePolicy = s"val policy = $policyString"
    interpreter.beQuietDuring({
      interpreter.interpret(completePolicy)
    })

    val somePolicy = interpreter.valueOfTerm("policy")

    somePolicy match {
      case None => throw new RuntimeException("Could not load policy from given policyString (returned None)")
      case Some(policy: AbstractPolicy) => policy
      case _ => throw new RuntimeException("Could not load policy from given policyString (result was not an AbstractPolicy)")
    }
  }
  
  /**
   * Parse the contents of the given file as a policy string and return 
   * the resulting policy.
   * 
   * More details: see parse(policyString: String)
   */
  def parseFile(path: String): AbstractPolicy = {
    val source = io.Source.fromFile(path)
    val policyString = source.mkString
    source.close
    parse(policyString)
  }
}
