/**
 *    Copyright 2014 KU Leuven Research and Developement - iMinds - Distrinet
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 *    Administrative Contact: dnet-project-office@cs.kuleuven.be
 *    Technical Contact: maarten.decat@cs.kuleuven.be
 *    Author: maarten.decat@cs.kuleuven.be
 */
package examples

import scala.swing._
import scala.swing.Swing._
import javax.swing.UIManager
import scala.swing.event.ButtonClicked
import java.awt.Font
import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.Settings
import stapl.core._
import stapl.core.pdp.PDP
import stapl.core.pdp.RequestCtx
import stapl.core.pdp.EvaluationCtx
import stapl.core.pdp.AttributeFinderModule
import stapl.core.pdp.AttributeFinder

/**
 * Example of a GUI that loads policies dynamically.
 */
object GUI extends SimpleSwingApplication {
  
  var prevPolicy = ""
  val settings = new Settings
  // add policy language class files/jars and libraries to bootclasspath
  settings.bootclasspath.append("/home/jasper/EclipseProjects/PolicyLanguage/bin")
  settings.bootclasspath.append("/home/jasper/Documents/joda-time-2.3/joda-time-2.3.jar:/home/jasper/Documents/joda-convert-1.6/joda-convert-1.6.jar")
  // make sure the same class loader is used
  settings.embeddedDefaults(this.getClass().getClassLoader())
  
  var pdp: PDP = null
  val finder = new AttributeFinder
  finder += MyFinder.module
  
  override def top = new MainFrame {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    title = "Policy Language Showcase"
      
    val subjectField = new TextField {
      preferredSize = (100,30)
    }
    val actionField = new TextField {
      preferredSize = (100,30)
    }
    val resourceField = new TextField {
      preferredSize = (100,30)
    }
    val button = new Button("Evaluate")
    val policyTxt = new TextArea {
      font = new Font(Font.MONOSPACED, 0, font.getSize())
    }
      
    contents = new SplitPane(Orientation.Vertical) {
      enabled = false
      border = EmptyBorder(10, 10, 10, 10)
      leftComponent = new BoxPanel(Orientation.Vertical) {
        contents += new FlowPanel(FlowPanel.Alignment.Left)() {
          contents += new Label("Subject:")
          contents += subjectField
          maximumSize = (200,30)
        }
        contents += new FlowPanel(FlowPanel.Alignment.Left)() {
          contents += new Label("Action:")
          contents += actionField
          maximumSize = (200,30)
        }
        contents += new FlowPanel(FlowPanel.Alignment.Left)() {
          contents += new Label("Resource:")
          contents += resourceField
          maximumSize = (200,30)
        }
        contents += new FlowPanel(FlowPanel.Alignment.Left)(){
          contents += button
        }
      }
      rightComponent = new ScrollPane(policyTxt) {
        preferredSize = (500,500)
      }
    }
    
    listenTo(button)
    
    reactions += {
      case ButtonClicked(b) => 
        val policyString = policyTxt.text
        val subject = subjectField.text
        val action = actionField.text
        val resource = resourceField.text

        if (policyString != prevPolicy) {
          val interpreter = new IMain(settings)
          interpreter.interpret("import be.kuleuven.cs.distrinet.policylang._\n\n" + policyString)
          val somePolicy = interpreter.valueOfTerm("policy")

          somePolicy match {
            case None                         => Dialog.showMessage(button, "An error occurred.", "Error", Dialog.Message.Error)
            case Some(policy: AbstractPolicy) =>
              prevPolicy = policyString
              pdp = new PDP(policy, finder)
              try {
                Dialog.showMessage(button, pdp.evaluate(new RequestCtx(subject, action, resource)), "Decision", Dialog.Message.Info)
              } catch {
                case e: Exception => Dialog.showMessage(button, e, "Error", Dialog.Message.Error)
              }
            case _                            =>
          }
        } else {
          try {
            Dialog.showMessage(button, pdp.evaluate(new RequestCtx(subject, action, resource)), "Decision", Dialog.Message.Info)
          } catch {
            case e: Exception => Dialog.showMessage(button, e, "Error", Dialog.Message.Error)
          }
        }
    }
  }

}

object MyFinder {
  val module: AttributeFinderModule = new MyModule
  
  class MyModule extends AttributeFinderModule {
    
    override def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, 
        aType: AttributeType, multiValued: Boolean): Option[ConcreteValue] = {
      // Note: we do not check the multiValued boolean here, the code below
      // knows these hard-coded
      cType match {
        case SUBJECT => name match {
          case "roles" => ctx.subjectId match {
            case "Jasper" => Some(List("student"))
            case "Sara" => Some(List("student"))
            case "Paul" => Some(List("teacher"))
            case _ => None
          }
          case "birthday" => ctx.subjectId match {
            case "Jasper" => Some(Day(1990, 2, 21))
            case "Sara" => Some(Day(1999, 3, 14))
            case "Paul" => Some(Day(1956, 7, 16))
            case _ => None
          }
          case _ => None
        }
        case RESOURCE => name match {
          case "category" => ctx.resourceId match {
            case "The Catcher in the Rye" => Some("book")
            case "Worst Case Scenario" => Some("cd")
            case _ => None
          }
          case _ => None
        }
        case ENVIRONMENT => name match {
          case "today" => {
            val date = new java.util.Date
            Some(Day(1900 + date.getYear(), date.getMonth() + 1, date.getDate()))
          }
          case _ => None
        }
        case _ => None
      }
    }
  }
}
