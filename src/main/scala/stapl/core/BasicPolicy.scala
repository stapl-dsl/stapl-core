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
