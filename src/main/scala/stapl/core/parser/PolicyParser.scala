package stapl.core.parser

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain
import stapl.core.AbstractPolicy

class PolicyParser {
  val settings = new Settings
  settings.usejavacp.value = true
  settings.nowarnings.value = true
  val interpreter = new IMain(settings)
  interpreter.beQuietDuring({
    interpreter.addImports(
      "stapl.core.{subject => _, resource => _, action => _, _}",
      "stapl.core.templates._")
  })
  
  def addImport(i: String) = {
    interpreter.beQuietDuring({
      interpreter.addImports(i)
    })
  }

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
  
  def parseFile(path: String): AbstractPolicy = {
    val source = io.Source.fromFile(path)
    val policyString = source.mkString
    source.close
    parse(policyString)
  }
}