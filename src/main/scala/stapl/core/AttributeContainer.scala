/*
 * Copyright 2015 Jasper Moeys, iMinds-DistriNet, KU Leuven
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
package stapl.core

import scala.language.experimental.macros
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.reflect.macros.blackbox.Context

class AttributeDeclarationException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause) 

/**
 * Base class for all attribute containers, such as the subject, resource, action and environment
 * in most STAPL policies.
 * 
 * Usage example: 
 * {{{
 * object subject extends Subject {
 *   val foo = SimpleAttribute(String)
 *   val bar = ListAttribute(Number)
 *   val baz = SimpleAttribute("bazz", String)
 * }
 * }}}
 * 
 * TODO mechanism is needed so attribute types are known (to the compiler) at compile time
 */
abstract class AttributeContainer (cType: AttributeContainerType, attributes: Map[String, Attribute[_]]) {

  final def this(cType: AttributeContainerType) = this(cType, Map())
  
  protected final def Attribute[T](name: String): Attribute[T] = {
    val attribute = new Attribute[T](cType, name)
    set(name, attribute)
    attribute
  }
  
  protected final def Attribute[T]: Attribute[T] = macro AttributeContainer.attributeMacro[T]
  
  final private def set(name: String, attribute: Attribute[_]) {
    if(attributes.contains(name)) {
      throw new AttributeDeclarationException(s"Error when assigning $cType.$name: already assigned")
    }
    attributes += name -> attribute   
  }
  
  final def get(name: String): Attribute[_] = {
    try attributes(name)
    catch {
      case _: NoSuchElementException => throw new AttributeDoesNotExistException(name)
    }    
  }
  
  def allAttributes: Seq[Attribute[_]] = attributes.values.toSeq
}

object AttributeContainer {
  
  def attributeMacro[T : c.WeakTypeTag](c: Context) = {
    import c.universe._
    val name = c.internal.enclosingOwner.fullName.split('.').last
    q"this.Attribute[${weakTypeOf[T]}]($name)"
  }
}

/**
 * All attribute container types for now.
 * 
 * TODO: extend this to support the container hierarchies as well?
 */
sealed abstract class AttributeContainerType
case object SUBJECT extends AttributeContainerType
case object RESOURCE extends AttributeContainerType
case object ENVIRONMENT extends AttributeContainerType
case object ACTION extends AttributeContainerType
