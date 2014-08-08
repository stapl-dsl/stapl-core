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
 * Package containing some policy templates which commonly occur in policies. 
 */
package object templates {

  /**
   * TODO Actually, this is DenyIffNot
   */
  def OnlyPermitIff(id: String)(target: Expression, condition: Expression): Policy =
    Policy(id) := when(target) apply PermitOverrides to (
      Rule("OnlyPermitIff-condition") := permit iff (condition),
      Rule("OnlyPermitIff-deny") := deny)

  /**
   * TODO Actually, this is PermitIffNot
   */
  def OnlyDenyIff(id: String)(target: Expression, condition: Expression): Policy =
    Policy(id) := when(target) apply DenyOverrides to (
      Rule("OnlyDenyIff-condition") := deny iff (condition),
      Rule("OnlyDenyIff-permit") := permit)

  /**
   * Default permit with default id.
   */
  def defaultPermit: Rule = defaultPermit("default-permit")

  /**
   * Always returns Permit on every request.
   */
  def defaultPermit(id: String): Rule =
    new Rule(id)(
      target = AlwaysTrue,
      effect = Permit,
      condition = AlwaysTrue)

  /**
   * Default deny with default id.
   */
  def defaultDeny: Rule = defaultDeny("default-deny")

  /**
   * Always returns Deny on every request.
   */
  def defaultDeny(id: String): Rule =
    new Rule(id)(
      target = AlwaysTrue,
      effect = Deny,
      condition = AlwaysTrue)

}