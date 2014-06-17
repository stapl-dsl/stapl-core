package stapl.core

trait Value {
  
  val aType: AttributeType
  
  val isList: Boolean
  
  protected[core] def getConcreteValue(ctx: EvaluationCtx): ConcreteValue
  
  private def typeCheck(that: Value) {
    AttributeType.checkType(that.aType, this.aType)
  }
  
  def in(that: Value): Expression = {
    if (this.isList || !that.isList)
      throw new UnsupportedOperationException("An in operation is only possible between a simple value and a list.")
    typeCheck(that)
    if (List(DateTimeDuration, DayDuration, TimeDuration) contains this.aType)
      throw new UnsupportedOperationException("An in operation is not possible for durations.")
    ValueIn(this, that)
  }
  
  /*def contains(that: Value): Expression = {
    if (!this.isList || that.isList)
      throw new UnsupportedOperationException("An in/contains operation is only possible between a list and a simple value.")
    typeCheck(that)
    if (List(DateTimeDuration, DayDuration, TimeDuration) contains this.aType)
      throw new UnsupportedOperationException("An in/contains operation is not possible for durations.")
    ValueIn(that, this)
  }*/
    
  
  def ===(that: Value): Expression = {
    if (this.isList || that.isList)
      throw new UnsupportedOperationException("An equals operation is only possible between simple values.")
    typeCheck(that)
    if (List(DateTimeDuration, DayDuration, TimeDuration) contains this.aType)
      throw new UnsupportedOperationException("An equals operation is not possible for durations.")
    EqualsValue(this, that)
  }
  
  def gt(that: Value): Expression = {
    if (this.isList || that.isList)
      throw new UnsupportedOperationException("A comparison operation is only possible between simple values.")
    typeCheck(that)
    if (List(DateTimeDuration, DayDuration, TimeDuration) contains this.aType)
      throw new UnsupportedOperationException("A comparison operation is not possible for durations.")
    GreaterThanValue(this, that)
  }
  
  def lt(that: Value): Expression = that gt this
  
  def gteq(that: Value): Expression = (this gt that) | (this === that)
  
  def lteq(that: Value): Expression = (this lt that) | (this === that)
  
  def +(that: Value): Operation = {
    Addition(this, that)
  }
  
  def -(that: Value): Operation = {
    Subtraction(this, that)
  }
  
  def *(that: Value): Operation = {
    Multiplication(this, that)
  }
  
  def /(that: Value): Operation = {
    Division(this, that)
  }
}
