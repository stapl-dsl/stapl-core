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

package stapl.core

import stapl.core.typeclasses.impl.Instances
import stapl.core.typeclasses.impl.Syntax

package object dsl extends DSL with Instances with Syntax with JodaTime {
  
  object any2stringadd // DIE any2stringadd DIE !!!
  
  /**
   * You might want to manually wrap something in a Value. Use with care.
   */
  @inline def Value[T](something: T) = stapl.core.Value.apply[T](something)
  

  val PermitOverrides = stapl.core.PermitOverrides
  val DenyOverrides = stapl.core.DenyOverrides
  val FirstApplicable = stapl.core.FirstApplicable
  val Permit = stapl.core.Permit
  val Deny = stapl.core.Deny
  
  
  private[dsl] abstract class SubjectTemplate extends AttributeContainer(SUBJECT) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class ResourceTemplate extends AttributeContainer(RESOURCE) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class ActionTemplate extends AttributeContainer(ACTION) {
    val id = Attribute[String]("id")
  }
  private[dsl] abstract class EnvironmentTemplate extends AttributeContainer(ENVIRONMENT)
  
  trait Subject extends SubjectTemplate
  trait Resource extends ResourceTemplate
  trait Action extends ActionTemplate
  trait Environment extends EnvironmentTemplate
  
  object templating {
    type Expression = stapl.core.Expression
    type Value[T] = stapl.core.Value[T]
  }
}
