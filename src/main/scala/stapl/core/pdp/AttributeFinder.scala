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
package stapl.core.pdp

import scala.annotation.tailrec
import scala.reflect.runtime.universe._
import stapl.core.Attribute
import stapl.core.AttributeContainerType
import stapl.core.AttributeNotFoundException
import stapl.core.AttributeType
import stapl.core.ConcreteValue
import stapl.core.ListAttribute
import stapl.core.SimpleAttribute
import stapl.core.TypeCheckException

/**
 * Class used for representing an attribute finder used by a PDP. An attribute
 * finder tries to find values for a certain value during policy evaluation.
 * An attribute finder contains multiple attribute finder modules that each
 * connect to a certain attribute value source, such as a hard-coded list, a file or
 * a database.
 */
sealed class AttributeFinder {
  type Modules = List[AttributeFinderModule]
  private var _modules: Modules = Nil
  
  def modules : Modules = _modules
  def modules_=(modules: Modules) {
    _modules = modules
  }
  
  /**
   * Add an attribute finder module to this attribute finder.
   */
  def addModule(module: AttributeFinderModule) {
    _modules = module :: _modules
  }
  
  /**
   * Short notation for adding attribute finder modules.
   */
  def +=(module: AttributeFinderModule) = addModule(module)
  
  /**
   * Tries to find the value of a certain attribute in the given evaluation context.
   * 
   * @throws	AttributeNotFoundException	If the attribute value isn't found
   * @throws	TypeCheckException 	If the type of the found value doesn't conform to the declared type of the attribute
   */
  @throws[AttributeNotFoundException]("if the attribute value isn't found")
  @throws[TypeCheckException]("if the type of the found value doesn't conform to the declared type of the attribute")
  def find(ctx: EvaluationCtx, attribute: Attribute): ConcreteValue = {
    @tailrec
    def find(modules: Modules): ConcreteValue = modules match {
      case module :: tail => module.find(ctx, attribute) match {
        case Some(result) => result
        case None => find(tail)
      }
      case Nil => throw new AttributeNotFoundException(attribute)
    }
    find(_modules)
  }
}

/**
 * Trait for all attribute finder modules passed to an attribute finder.
 */
trait AttributeFinderModule {
  
  /**
   * The public method for trying to find the value of a certain attribute.
   * 
   * TODO why is this private[core]?
   */
  private[core] def find(ctx: EvaluationCtx, attribute: Attribute): Option[ConcreteValue] = attribute match {
    case SimpleAttribute(cType,name,aType) => checkTypeSimple(find(ctx, cType, name, aType), aType)
    case ListAttribute(cType,name,aType) => checkTypeList(find(ctx, cType, name, aType), aType)
  }
  
  /**
   * Check the type of the given value.
   */
  private def checkTypeSimple(result: Option[ConcreteValue], aType: AttributeType): Option[ConcreteValue] = {
    result match {
      case Some(value) => {
        require(!value.isList, "This value should not be a list.")
        AttributeType.checkType(value.aType, aType)
      }
      case None => 
    }
    result
  }
  
  /**
   * Check the type of the given value.
   */
  private def checkTypeList(result: Option[ConcreteValue], aType: AttributeType): Option[ConcreteValue] = {
    result match {
      case Some(value) => {
        require(value.isList, "This value should be a list.")
        AttributeType.checkType(value.aType, aType)
      }
      case None => 
    }
    result
  }
  
  /**
   * The actual implementation for trying to find the value of a certain attribute.
   */
  protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType): Option[ConcreteValue]
  
}