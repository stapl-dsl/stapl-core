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

import scala.collection.mutable.Map

/**
 * Class used for representing the result of a policy evaluation by a PDP.
 * A result contains a decision and possibly obligation actions to be fulfilled.
 */
case class Result(val decision: Decision, 
    val obligationActions: List[ConcreteObligationAction] = List.empty, 
    val employedAttributes: Map[Attribute,ConcreteValue] = Map())

/**
 * Trait for representing a Decision ( = Permit, Deny or NotApplicable).
 */
sealed trait Decision

/**
 * Trait for representing the Effect of a Rule. 
 * Only two Decisions are also Effects: Permit and Deny
 */
sealed trait Effect extends Decision {
  
  /**
   * Returns the reverse of this effect: Permit -> Deny and Deny -> Permit.
   */
  def reverse(): Effect = this match {
    case Permit => Deny
    case Deny => Permit
  }
}

case object Permit extends Effect
  
case object Deny extends Effect

case object NotApplicable extends Decision