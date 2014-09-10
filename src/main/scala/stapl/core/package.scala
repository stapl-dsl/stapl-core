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
package stapl

import scala.language.implicitConversions
import stapl.core.SimpleAttribute
import org.joda.time.LocalDateTime

package object core {
  
  /**
   * Some implicits for converting standard Scala types to STAPL values. 
   */
  
  implicit def boolean2Value(boolean: Boolean): ConcreteValue = new BoolImpl(boolean)
  
  implicit def int2Value(int: Int): ConcreteValue = new NumberImpl(Left(int))
  
  implicit def double2Value(double: Double): ConcreteValue = new NumberImpl(Right(double))
  
  implicit def long2Value(long: Long): ConcreteValue = new NumberImpl(Left(long))
  
  implicit def string2Value(string: String): ConcreteValue = new StringImpl(string)
  
  implicit def dateTime2Value(dt: LocalDateTime): ConcreteValue = new DateTimeImpl(dt)
  
  implicit def stringSeq2Value(seq: Seq[String]): ConcreteValue = new StringSeqImpl(seq)
  
  implicit def booleanSeq2Value(seq: Seq[Boolean]): ConcreteValue = new BoolSeqImpl(seq)
  
  implicit def intSeq2Value(seq: Seq[Int]): ConcreteValue = new IntSeqImpl(seq)
  
  implicit def doubleSeq2Value(seq: Seq[Double]): ConcreteValue = new DoubleSeqImpl(seq)
  
  implicit def longSeq2Value(seq: Seq[Long]): ConcreteValue = new LongSeqImpl(seq)
  
  implicit def jodaDateTimeSeq2Value(seq: Seq[LocalDateTime]): ConcreteValue = new DateTimeSeqImpl(seq.map(l => new DateTimeImpl(l)))
  
  implicit def dateTimeSeq2Value(seq: Seq[DateTimeImpl]): ConcreteValue = new DateTimeSeqImpl(seq)
  
  implicit def timeSeq2Value(seq: Seq[TimeImpl]): ConcreteValue = new TimeSeqImpl(seq)
  
  implicit def daySeq2Value(seq: Seq[DayImpl]): ConcreteValue = new DaySeqImpl(seq)
  
  implicit def dateTimeDurSeq2Value(seq: Seq[DateTimeDurationImpl]): ConcreteValue = new DateTimeDurSeqImpl(seq)
  
  implicit def timeDurSeq2Value(seq: Seq[TimeDurationImpl]): ConcreteValue = new TimeDurSeqImpl(seq)
  
  implicit def dayDurSeq2Value(seq: Seq[DayDurationImpl]): ConcreteValue = new DayDurSeqImpl(seq)
  
  /**
   * Some implicits for converting standard Scala types to STAPL expressions. 
   */
    
  implicit def boolAttributeToExpression(attribute: Attribute): Expression = attribute match {
    case x@SimpleAttribute(_,_,Bool) => BoolExpression(x)
    case SimpleAttribute(_,_,aType) => throw new TypeCheckException(aType, Bool)
    case _ => throw new IllegalArgumentException("Found a list, but expected a Bool.")
  }
  
  implicit def boolean2Expression(bool: Boolean): Expression = if(bool) AlwaysTrue else AlwaysFalse
  
  implicit def int2DurationBuilder(int: Int) = new DurationBuilder(int)
    
  def abs(value: Value): Operation = AbsoluteValue(value)
  
  /**
   * Implicit for converting a Decision to a Result without obligations.
   */
  implicit def decision2Result(decision: Decision): Result = Result(decision)
  
  /**
   * Implicit for the natural policy language.
   */  
  implicit def obligationAction2ObligationActionWithOn(oa: ObligationAction): ObligationActionWithOn = new ObligationActionWithOn(oa)
	
  
  /**
   * The definitions of the standard subject, action, resource and environment.
   * 
   * Important: always create a new instance so multiple policies can work on
   * multiple instances of these objects, but testing equality with the id attribute
   * of every instance will always work.
   * 
   * TODO remove these default objects?
   */
  def subject: SubjectAttributeContainer = {
    val subject = new SubjectAttributeContainer
    subject.id = SimpleAttribute(String)
    subject
  }
  def resource: ResourceAttributeContainer = {
    val resource = new ResourceAttributeContainer
    resource.id = SimpleAttribute(String)
    resource
  }
  def action: ActionAttributeContainer = {
    val action = new ActionAttributeContainer
    action.id = SimpleAttribute(String)
    action
  }
  def environment: EnvironmentAttributeContainer = new EnvironmentAttributeContainer
}