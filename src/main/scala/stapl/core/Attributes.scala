package stapl.core

import AttributeType._
import stapl.core.pdp.EvaluationCtx

sealed abstract class Attribute(cType: AttributeContainerType, name: String, aType: AttributeType) extends Value {
  
  override def getConcreteValue(ctx: EvaluationCtx) = 
    ctx.findAttribute(this)
}



case class ListAttribute(cType: AttributeContainerType, name: String, aType: AttributeType) 
  extends Attribute(cType, name, aType) {
  
  override val isList = true
}

object ListAttribute {
  import AttributeConstruction.UninitializedAttribute
  def apply(sType: AttributeType): UninitializedAttribute = 
    (None, new ListAttribute(_: AttributeContainerType, _: String, sType))
  
  def apply(name: String, sType: AttributeType): UninitializedAttribute = 
    (Option(name), new ListAttribute(_: AttributeContainerType, _: String, sType))
}



case class SimpleAttribute(cType: AttributeContainerType, name: String, aType: AttributeType) 
  extends Attribute(cType, name, aType) {
  
  override val isList = false
  
}

object SimpleAttribute {
  import AttributeConstruction.UninitializedAttribute
  def apply(sType: AttributeType): UninitializedAttribute = 
    (None, new SimpleAttribute(_: AttributeContainerType, _: String, sType))
  
  def apply(name: String, sType: AttributeType): UninitializedAttribute = 
    (Option(name), new SimpleAttribute(_: AttributeContainerType, _: String, sType))
}



object AttributeConstruction {
  private type AttributeConstructor = (AttributeContainerType, String) => Attribute
  type UninitializedAttribute = (Option[String], AttributeConstructor)
}
