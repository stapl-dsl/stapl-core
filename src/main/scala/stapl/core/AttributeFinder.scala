package stapl.core

import scala.annotation.tailrec
import scala.reflect.runtime.universe._

sealed class AttributeFinder {
  type Modules = List[AttributeFinderModule]
  private var _modules: Modules = Nil
  
  def modules : Modules = _modules
  def modules_=(modules: Modules) {
    _modules = modules
  }
  
  def addModule(module: AttributeFinderModule) {
    _modules = module :: _modules
  }
  
  def +=(module: AttributeFinderModule) = addModule(module)
  
  @throws[AttributeNotFoundException]("if the attribute isn't found")
  @throws[TypeCheckException]("if the type of the found value doesn't conform to the declared type of the attribute")
  def find(ctx: EvaluationCtx, attribute: Attribute): ConcreteValue = {
    @tailrec
    def find(modules: Modules): ConcreteValue = modules match {
      case module :: tail => module.find(ctx, attribute) match {
        case Some(result) => result
        case None => find(tail)
      }
      case Nil => throw new AttributeNotFoundException(attribute)
    }
    find(_modules)
  }
}

trait AttributeFinderModule {
  private[core] def find(ctx: EvaluationCtx, attribute: Attribute): Option[ConcreteValue] = attribute match {
    case SimpleAttribute(cType,name,aType) => checkTypeSimple(find(ctx, cType, name, aType), aType)
    case ListAttribute(cType,name,aType) => checkTypeList(find(ctx, cType, name, aType), aType)
  }
  
  private def checkTypeSimple(result: Option[ConcreteValue], aType: AttributeType): Option[ConcreteValue] = {
    result match {
      case Some(value) => {
        require(!value.isList, "This value should not be a list.")
        AttributeType.checkType(value.aType, aType)
      }
      case None => 
    }
    result
  }
  
  private def checkTypeList(result: Option[ConcreteValue], aType: AttributeType): Option[ConcreteValue] = {
    result match {
      case Some(value) => {
        require(value.isList, "This value should be a list.")
        AttributeType.checkType(value.aType, aType)
      }
      case None => 
    }
    result
  }
  
  protected def find(ctx: EvaluationCtx, cType: AttributeContainerType, name: String, aType: AttributeType): Option[ConcreteValue]
  
}