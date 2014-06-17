package stapl.core

import scala.collection.mutable.Map

class RequestCtx(val subjectID: String, val actionID: String, val resourceID: String) {

  final val attributes: Map[Attribute,ConcreteValue] = Map(SimpleAttribute(SUBJECT, "id", String) -> subjectID,
                                                           SimpleAttribute(RESOURCE, "id", String) -> resourceID,
                                                           SimpleAttribute(ACTION, "id", String) -> actionID)
}