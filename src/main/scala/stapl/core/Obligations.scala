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

/**
 * 
 */
case class Obligation(val action: ObligationAction, val fulfillOn: Effect)

/**
 * Class used for representing the action to be performed as specified in
 * an obligation.
 */
abstract class ObligationAction // TODO better name for this? very long...

case class LogObligationAction(val msg: Value) extends ObligationAction
object log {
  def apply(msg: Value) = new LogObligationAction(msg)
}

case class MailObligationAction(val to: String, val msg: String) extends ObligationAction
object mail {
  def apply(to: String, msg: String) = new MailObligationAction(to, msg)
}

case class UpdateAttributeObligationAction(val attribute: Attribute, val value: ConcreteValue) extends ObligationAction
object update {
  def apply(attribute: Attribute, value: ConcreteValue) =
    new UpdateAttributeObligationAction(attribute, value)
}