package stapl.core

import scala.language.dynamics
import scala.collection.mutable.Map
import scala.collection.mutable.Buffer


class AttributeContainer private(cType: AttributeContainerType, attributes: Map[String, Attribute]) extends Dynamic {

  final def this(cType: AttributeContainerType) = this(cType, Map("id" -> SimpleAttribute(cType, "id", String)))
  
  // import the type alias for uninitialized attributes
  import AttributeConstruction.UninitializedAttribute
  
  final def selectDynamic(name: String): Attribute =
    try attributes(name)
    catch {
      case _: NoSuchElementException => throw new AttributeDoesNotExistException(name)
    }
  
  final def updateDynamic(name: String)(attribute: UninitializedAttribute){
    val (optionName, attributeConstructor) = attribute
    val actualAttribute = optionName match {
      case Some(someName) => attributeConstructor(cType, someName)
      case None => attributeConstructor(cType, name)
    }
    attributes += name -> actualAttribute
    refinements.foreach(_.updateDynamic(name)(attribute))
  }
  
  private val refinements = Buffer.empty[AttributeContainer]
  
  final def refine(): AttributeContainer = {
    val refinement = new AttributeContainer(cType, attributes.clone)
    refinements += refinement
    refinement
  }
}


sealed abstract class AttributeContainerType
case object SUBJECT extends AttributeContainerType
case object RESOURCE extends AttributeContainerType
case object ENVIRONMENT extends AttributeContainerType
case object ACTION extends AttributeContainerType