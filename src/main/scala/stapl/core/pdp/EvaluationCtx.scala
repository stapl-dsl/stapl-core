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

import grizzled.slf4j.Logging
import stapl.core.ConcreteValue
import stapl.core.Attribute
import stapl.core.AttributeContainerType
import stapl.core.AttributeNotFoundException
import stapl.core.PermitOverrides
import stapl.core.DenyOverrides
import stapl.core.FirstApplicable
import stapl.core.CombinationAlgorithm
import stapl.core.CombinationAlgorithmImplementation
import stapl.core.CombinationAlgorithmImplementationBundle
import stapl.core.SimpleCombinationAlgorithmImplementationBundle
import scala.concurrent.Future
import scala.concurrent.blocking
import scala.concurrent.ExecutionContext.Implicits.global
import stapl.core.CombinationAlgorithmImplementationBundle
import scala.util.{ Try, Success, Failure }
import stapl.core.SUBJECT
import stapl.core.RESOURCE
import stapl.core.ACTION
import stapl.core.ENVIRONMENT

/**
 * The base class of the context for evaluating a policy. This context
 * represents all information for that policy evaluation, such as the
 * id of the subject, the id of the resource, the id of the action and
 * a method to find attributes.
 *
 * The method to find attributes is required in the evaluation context
 * because certain aspects such as an attribute cache are specific for
 * each individual evaluation context.
 */
trait EvaluationCtx {

  def evaluationId: String
  def subjectId: String
  def resourceId: String
  def actionId: String
  def remoteEvaluator: RemoteEvaluator
  def cachedAttributes: Map[Attribute[_], Any]
  def employedAttributes: Map[Attribute[_], Any]
  protected[core] def findAttribute[T](attribute: Attribute[T]): T
  protected[core] def getCombinationAlgorithmImplementation(algo: CombinationAlgorithm): CombinationAlgorithmImplementation

  // TODO add type checking here
  //final def findAttribute(attribute: Attribute): ConcreteValue = 
}

/**
 * An implementation of a basic evaluation context. This evaluation context
 * stores the subject id, the resource id, the action id and stores found
 * attribute values in a cache for this evaluation context.
 */
class BasicEvaluationCtx(override val evaluationId: String, request: RequestCtx,
  finder: AttributeFinder, override val remoteEvaluator: RemoteEvaluator,
  bundle: CombinationAlgorithmImplementationBundle = SimpleCombinationAlgorithmImplementationBundle) extends EvaluationCtx with Logging {

  override val subjectId: String = request.subjectId

  override val resourceId: String = request.resourceId

  override val actionId: String = request.actionId

  protected val attributeCache: scala.collection.mutable.Map[Attribute[_], Any] = scala.collection.mutable.Map() //scala.collection.concurrent.TrieMap()

  override def cachedAttributes: Map[Attribute[_], Any] = attributeCache.toMap

  // add all attributes given in the request to the attribute cache
  for ((attribute, value) <- request.allAttributes) {
    attributeCache(attribute) = value
  }

  protected val _employedAttributes: scala.collection.mutable.Map[Attribute[_], Any] = scala.collection.mutable.Map()

  override def employedAttributes = _employedAttributes.toMap

  /**
   * Try to find the value of the given attribute. If the value is already
   * in the attribute cache, that value is returned. Otherwise, the attribute
   * finder is checked and the found value is stored in the attribute cache if
   * a value is found.
   *
   * @throws	AttributeNotFoundException	If the attribute value isn't found
   */
  @throws[AttributeNotFoundException]("if the attribute value isn't found")
  override def findAttribute[T](attribute: Attribute[T]): T = {
    attributeCache.get(attribute) match {
      case Some(value) => {
        debug("FLOW: found value of " + attribute + " in cache: " + value)
        _employedAttributes(attribute) = value
        value.asInstanceOf[T]
      }
      case None => { // Not in the cache
        finder.find(this, attribute) match {
          case None =>
            val entityId = attribute.cType match {
              case SUBJECT => subjectId
              case RESOURCE => resourceId
              case ACTION => "ACTION??" // we don't support this
              case ENVIRONMENT => "ENVIRONMENT??" // we don't support this
            }
            debug(s"Didn't find value of $attribute for entity $entityId anywhere, throwing exception")
            throw new AttributeNotFoundException(evaluationId, entityId, attribute)
          case Some(value) =>
            attributeCache(attribute) = value // add to cache
            _employedAttributes(attribute) = value
            debug("FLOW: retrieved value of " + attribute + ": " + value + " and added to cache")
            value.asInstanceOf[T]
        }
      }
    }
  }

  /**
   * Return the implementation of the requested combination algorithm.
   */
  def getCombinationAlgorithmImplementation(algo: CombinationAlgorithm): CombinationAlgorithmImplementation = algo match {
    case PermitOverrides => bundle.PermitOverrides
    case DenyOverrides => bundle.DenyOverrides
    case FirstApplicable => bundle.FirstApplicable
  }
}