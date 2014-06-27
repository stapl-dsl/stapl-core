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

import java.util.Date


sealed abstract class AttributeType {
  
  def addition(t: AttributeType): Option[AttributeType] = None
  def subtraction(t: AttributeType): Option[AttributeType] = None
  def multiplication(t: AttributeType): Option[AttributeType] = None
  def division(t: AttributeType): Option[AttributeType] = None
  def absoluteValue(): Option[AttributeType] = None
}

object AttributeType {
  
  @throws[TypeCheckException]("if type found doesn't conform to type expected")
  def checkType(found: AttributeType, expected: AttributeType) {
    if(found != expected) throw new TypeCheckException(found, expected)
  }
}



case object String extends AttributeType {
  
  override def addition(t: AttributeType): Option[AttributeType] = t match {
    case String => Some(String)
    case _ => None
  }
}

case object Number extends AttributeType {
  
  override def addition(t: AttributeType): Option[AttributeType] = t match {
    case Number => Some(Number)
    case _ => None
  }
  
  override def subtraction(t: AttributeType): Option[AttributeType] = t match {
    case Number => Some(Number)
    case _ => None
  }
  
  override def multiplication(t: AttributeType): Option[AttributeType] = t match {
    case Number => Some(Number)
    case _ => None
  }
  
  override def division(t: AttributeType): Option[AttributeType] = t match {
    case Number => Some(Number)
    case _ => None
  }
  
  override def absoluteValue(): Option[AttributeType] = Some(Number)
}

case object Bool extends AttributeType


case object DateTime extends AttributeType {
  def apply(year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int, millis:Int) : DateTimeImpl =
    new DateTimeImpl(year,month,day,hours,minutes,seconds,millis)
  
  def apply(year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int) : DateTimeImpl =
    apply(year, month, day, hours, minutes, seconds, 0)
  
    
  override def addition(t: AttributeType): Option[AttributeType] = t match {
    case DateTimeDuration => Some(DateTime)
    case DayDuration => Some(DateTime)
    case TimeDuration => Some(DateTime)
    case _ => None
  }
  
  override def subtraction(t: AttributeType): Option[AttributeType] = t match {
    case DateTimeDuration => Some(DateTime)
    case DayDuration => Some(DateTime)
    case TimeDuration => Some(DateTime)
    case DateTime => Some(DateTimeDuration)
    case _ => None
  }
}

case object Time extends AttributeType {
  def apply(hours:Int, minutes:Int, seconds:Int, millis:Int) : TimeImpl = 
    new TimeImpl(hours,minutes,seconds,millis)
  
  def apply(hours:Int, minutes:Int, seconds:Int) : TimeImpl = 
    new TimeImpl(hours,minutes,seconds,0)
  
  
  override def addition(t: AttributeType): Option[AttributeType] = t match {
    case TimeDuration => Some(Time)
    case _ => None
  }
  
  override def subtraction(t: AttributeType): Option[AttributeType] = t match {
    case TimeDuration => Some(Time)
    case Time => Some(TimeDuration)
    case _ => None
  }
}

case object Day extends AttributeType {
  def apply(year:Int, month:Int, day:Int): DayImpl = new DayImpl(year,month,day)
  
  
  override def addition(t: AttributeType): Option[AttributeType] = t match {
    case DayDuration => Some(Day)
    case _ => None
  }
  
  override def subtraction(t: AttributeType): Option[AttributeType] = t match {
    case DayDuration => Some(Day)
    case Day => Some(DayDuration)
    case _ => None
  }
}


case object DateTimeDuration extends AttributeType {
  def apply(years: Int, months: Int, days: Int, hours: Int, minutes: Int, seconds: Int, millis: Int): DateTimeDurationImpl =
    new DateTimeDurationImpl(years,months,days,hours,minutes,seconds,millis)
  
  def apply(years: Int, months: Int, days: Int, hours: Int, minutes: Int, seconds: Int): DateTimeDurationImpl =
    apply(years, months, days, hours, minutes, seconds, 0)
  
    
  override def addition(t: AttributeType): Option[AttributeType] = t match {
    case DateTimeDuration => Some(DateTimeDuration)
    case DayDuration => Some(DateTimeDuration)
    case TimeDuration => Some(DateTimeDuration)
    case _ => None
  }
  
  override def subtraction(t: AttributeType): Option[AttributeType] = t match {
    case DateTimeDuration => Some(DateTimeDuration)
    case DayDuration => Some(DateTimeDuration)
    case TimeDuration => Some(DateTimeDuration)
    case _ => None
  }
}

case object TimeDuration extends AttributeType {
  def apply(hours: Int, minutes: Int, seconds: Int, millis: Int): TimeDurationImpl = 
    new TimeDurationImpl(hours,minutes,seconds,millis)
  
  def apply(hours: Int, minutes: Int, seconds: Int): TimeDurationImpl = 
    apply(hours,minutes,seconds,0)
  
  
  override def addition(t: AttributeType): Option[AttributeType] = t match {
    case DateTimeDuration => Some(DateTimeDuration)
    case DayDuration => Some(DateTimeDuration)
    case TimeDuration => Some(TimeDuration)
    case _ => None
  }
  
  override def subtraction(t: AttributeType): Option[AttributeType] = t match {
    case DateTimeDuration => Some(DateTimeDuration)
    case DayDuration => Some(DateTimeDuration)
    case TimeDuration => Some(TimeDuration)
    case _ => None
  }
}

case object DayDuration extends AttributeType {
  def apply(years:Int, months:Int, days:Int): DayDurationImpl = new DayDurationImpl(years,months,days)
  
  
  override def addition(t: AttributeType): Option[AttributeType] = t match {
    case DateTimeDuration => Some(DateTimeDuration)
    case DayDuration => Some(DayDuration)
    case TimeDuration => Some(DateTimeDuration)
    case _ => None
  }
  
  override def subtraction(t: AttributeType): Option[AttributeType] = t match {
    case DateTimeDuration => Some(DateTimeDuration)
    case DayDuration => Some(DayDuration)
    case TimeDuration => Some(DateTimeDuration)
    case _ => None
  }
}
