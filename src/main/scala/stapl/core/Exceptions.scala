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

class TypeCheckException(message: String) extends RuntimeException(message) {
  
  def this(found: AttributeType, expected: AttributeType) = this(s"Found AttributeType '$found', but expected AttributeType '$expected'.")
}

class AttributeNotFoundException(attribute: Attribute) extends RuntimeException(s"$attribute wasn't found.")

class AttributeDoesNotExistException(name: String) extends RuntimeException(s"No attribute with name '$name' has been defined.")