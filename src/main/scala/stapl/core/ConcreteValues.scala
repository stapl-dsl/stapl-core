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

import org.joda.time.LocalTime
import org.joda.time.LocalDate
import org.joda.time.LocalDateTime
import org.joda.time.Period
import stapl.core.pdp.EvaluationCtx

object ConcreteValue {
  
  def unapply(value: Value)(implicit ctx: EvaluationCtx): Option[ConcreteValue] =
    Option(value.getConcreteValue(ctx))
}

object Representation {
  
  def unapply(value: Value)(implicit ctx: EvaluationCtx): Option[Any] =
    Option(value.getConcreteValue(ctx).representation)
}

sealed trait ConcreteValue extends Value with Serializable {
  
  override def getConcreteValue(ctx: EvaluationCtx): ConcreteValue = this
  
  val representation: Any
  
  def equalRepr(value: ConcreteValue): Boolean = this.representation == value.representation
  
  def reprGreaterThan(value: ConcreteValue): Boolean = throw new UnsupportedOperationException
  
  def reprContains(value: ConcreteValue): Boolean = throw new UnsupportedOperationException
  
  def add(value: ConcreteValue): ConcreteValue = throw new UnsupportedOperationException
  
  def subtract(value: ConcreteValue): ConcreteValue = throw new UnsupportedOperationException
  
  def multiply(value: ConcreteValue): ConcreteValue = throw new UnsupportedOperationException
  
  def divide(value: ConcreteValue): ConcreteValue = throw new UnsupportedOperationException
  
  def abs(): ConcreteValue = throw new UnsupportedOperationException
}

/*sealed abstract class AbstractDateTime(year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int, val nanoseconds:Int)
  extends ConcreteValue with Ordered[AbstractDateTime] with Equals {
  
  require(nanoseconds >= 0 && nanoseconds < 1000000000)
  
  val date : Date = Utilities.constructDate(year, month, day, hours, minutes, seconds)
  
  override val isList = false
  
  override val representation = this
  
  override def compare(that: AbstractDateTime) : Int = {
    val dateComp = this.date compareTo that.date
    if (dateComp != 0)
      dateComp
    else
      this.nanoseconds - that.nanoseconds
  }
  
  def canEqual(other: Any): Boolean
  
  override def equals(other: Any) = {
    other match {
      case that: be.kuleuven.cs.distrinet.policylang.AbstractDateTime => that.canEqual(AbstractDateTime.this) && nanoseconds == that.nanoseconds
      case _ => false
    }
  }
  
  override def hashCode() = {
    val prime = 41
    prime * (prime + date.hashCode) + nanoseconds.hashCode
  }
  
  
  override def reprGreaterThan(value: ConcreteValue): Boolean = 
    this.representation > value.representation.asInstanceOf[AbstractDateTime]
}
*/
class DateTimeImpl(val dt: LocalDateTime)
      extends ConcreteValue with Equals {
  
  def this(year:Int, month:Int, day:Int, hours:Int, minutes:Int, seconds:Int, millis:Int) = 
    this(new LocalDateTime(year, month, day, hours, minutes, seconds, millis))
  
  override val representation = this
  
  override val isList = false
  
  override val aType = DateTime
  
  override def toString(): String = dt.toString()
  
  def canEqual(other: Any) = {
    other.isInstanceOf[DateTimeImpl]
  }
  
  override def equals(other: Any) = {
    other match {
      case that: DateTimeImpl => that.canEqual(this) && this.dt == that.dt
      case _ => false
    }
  }
  
  override def hashCode() = {
    val prime = 41
    prime + dt.hashCode()
  }
  
  override def reprGreaterThan(value: ConcreteValue): Boolean = 
    (this.dt compareTo value.asInstanceOf[DateTimeImpl].dt) > 0
  
  override def add(value: ConcreteValue): ConcreteValue = value match {
      case d: DateTimeDurationImpl => new DateTimeImpl(this.dt.plus(d.period))
      case d: DayDurationImpl => new DateTimeImpl(this.dt.plus(d.period))
      case d: TimeDurationImpl => new DateTimeImpl(this.dt.plus(d.period))
      case _ => throw new UnsupportedOperationException
    }
  
  override def subtract(value: ConcreteValue): ConcreteValue = value match {
      case d: DateTimeDurationImpl => new DateTimeImpl(this.dt.minus(d.period))
      case d: DayDurationImpl => new DateTimeImpl(this.dt.minus(d.period))
      case d: TimeDurationImpl => new DateTimeImpl(this.dt.minus(d.period))
      case d: DateTimeImpl => new DateTimeDurationImpl(new Period(this.dt, d.dt))
      case _ => throw new UnsupportedOperationException
    }
}

class TimeImpl(val time: LocalTime) extends ConcreteValue with Equals {
  
  def this(hours:Int, minutes:Int, seconds:Int, millis:Int) = 
    this(new LocalTime(hours, minutes, seconds, millis))
  
  override val representation = this
  
  override val isList = false
  
  override val aType = Time
  
  override def toString(): String = time.toString()
  
  def canEqual(other: Any) = {
    other.isInstanceOf[TimeImpl]
  }
  
  override def equals(other: Any) = {
    other match {
      case that: TimeImpl => that.canEqual(this) && this.time == that.time
      case _ => false
    }
  }
  
  override def hashCode() = {
    val prime = 41
    prime + time.hashCode()
  }
  
  override def reprGreaterThan(value: ConcreteValue): Boolean = 
    (this.time compareTo value.asInstanceOf[TimeImpl].time) > 0
  
  override def add(value: ConcreteValue): ConcreteValue = value match {
      case d: TimeDurationImpl => new TimeImpl(this.time.plus(d.period))
      case _ => throw new UnsupportedOperationException
    }
    
  override def subtract(value: ConcreteValue): ConcreteValue = value match {
      case d: TimeDurationImpl => new TimeImpl(this.time.minus(d.period))
      case d: TimeImpl => new TimeDurationImpl(new Period(this.time, d.time))
      case _ => throw new UnsupportedOperationException
    }
}

class DayImpl(val day: LocalDate) extends ConcreteValue with Equals {
  
  def this(year:Int, month:Int, day:Int) =
    this(new LocalDate(year, month, day))
  
  override val representation = this
  
  override val isList = false
  
  override val aType = Day
  
  override def toString(): String = day.toString()
  
  def canEqual(other: Any) = {
    other.isInstanceOf[DayImpl]
  }
  
  override def equals(other: Any) = {
    other match {
      case that: DayImpl => that.canEqual(this) && this.day == that.day
      case _ => false
    }
  }
  
  override def hashCode() = {
    val prime = 41
    prime + day.hashCode()
  }
  
  override def reprGreaterThan(value: ConcreteValue): Boolean = 
    (this.day compareTo value.asInstanceOf[DayImpl].day) > 0
  
  override def add(value: ConcreteValue): ConcreteValue = value match {
      case d: DayDurationImpl => new DayImpl(this.day.plus(d.period))
      case _ => throw new UnsupportedOperationException
    }
  
  override def subtract(value: ConcreteValue): ConcreteValue = value match {
      case d: DayDurationImpl => new DayImpl(this.day.minus(d.period))
      case d: DayImpl => new DayDurationImpl(new Period(this.day, d.day))
      case _ => throw new UnsupportedOperationException
    }
}


abstract class Duration(val period: Period) extends ConcreteValue {
  
  override val representation = this
  
  override val isList = false
  
  override def toString(): String = period.toString()
}

class DateTimeDurationImpl(period: Period) extends Duration(period) {
  
  def this(years:Int, months:Int, days:Int, hours:Int, minutes:Int, seconds:Int, millis:Int) =
    this(Period.years(years)
               .withMonths(months)
               .withDays(days)
               .withHours(hours)
               .withMinutes(minutes)
               .withSeconds(seconds)
               .withMillis(millis))
  
  override val aType = DateTimeDuration
  
  override def add(value: ConcreteValue): ConcreteValue = value match {
      case d: DateTimeDurationImpl => new DateTimeDurationImpl(this.period.plus(d.period))
      case d: DayDurationImpl => new DateTimeDurationImpl(this.period.plus(d.period))
      case d: TimeDurationImpl => new DateTimeDurationImpl(this.period.plus(d.period))
      case _ => throw new UnsupportedOperationException
    }
  
  override def subtract(value: ConcreteValue): ConcreteValue = value match {
      case d: DateTimeDurationImpl => new DateTimeDurationImpl(this.period.minus(d.period))
      case d: DayDurationImpl => new DateTimeDurationImpl(this.period.minus(d.period))
      case d: TimeDurationImpl => new DateTimeDurationImpl(this.period.minus(d.period))
      case _ => throw new UnsupportedOperationException
    }
}

class TimeDurationImpl(period: Period) extends Duration(period) {
  
  def this(hours:Int, minutes:Int, seconds:Int, millis:Int) = 
    this(Period.hours(hours)
               .withMinutes(minutes)
               .withSeconds(seconds)
               .withMillis(millis))
  
  override val aType = TimeDuration
  
  override def add(value: ConcreteValue): ConcreteValue = value match {
      case d: DateTimeDurationImpl => new DateTimeDurationImpl(this.period.plus(d.period))
      case d: DayDurationImpl => new DateTimeDurationImpl(this.period.plus(d.period))
      case d: TimeDurationImpl => new TimeDurationImpl(this.period.plus(d.period))
      case _ => throw new UnsupportedOperationException
    }
  
  override def subtract(value: ConcreteValue): ConcreteValue = value match {
      case d: DateTimeDurationImpl => new DateTimeDurationImpl(this.period.minus(d.period))
      case d: DayDurationImpl => new DateTimeDurationImpl(this.period.minus(d.period))
      case d: TimeDurationImpl => new TimeDurationImpl(this.period.minus(d.period))
      case _ => throw new UnsupportedOperationException
    }
}

class DayDurationImpl(period: Period) extends Duration(period) {
  
  def this(years:Int, months:Int, days:Int) =
    this(Period.years(years)
               .withMonths(months)
               .withDays(days))
  
  override val aType = DayDuration
  
  override def add(value: ConcreteValue): ConcreteValue = value match {
      case d: DateTimeDurationImpl => new DateTimeDurationImpl(this.period.plus(d.period))
      case d: DayDurationImpl => new DayDurationImpl(this.period.plus(d.period))
      case d: TimeDurationImpl => new DateTimeDurationImpl(this.period.plus(d.period))
      case _ => throw new UnsupportedOperationException
    }
  
  override def subtract(value: ConcreteValue): ConcreteValue = value match {
      case d: DateTimeDurationImpl => new DateTimeDurationImpl(this.period.minus(d.period))
      case d: DayDurationImpl => new DayDurationImpl(this.period.minus(d.period))
      case d: TimeDurationImpl => new DateTimeDurationImpl(this.period.minus(d.period))
      case _ => throw new UnsupportedOperationException
    }
}


sealed abstract class BasicValue(private val value: Any, override val aType: AttributeType) extends ConcreteValue with Equals {
  
  override val isList = false
  
  override val representation = value
  
  override def toString(): String = value.toString
  
  override def equals(other: Any) = {
    other match {
      case that: BasicValue => that.canEqual(this) && value == that.value
      case _ => false
    }
  }
  
  override def hashCode() = {
    val prime = 41
    prime + value.##
  }
  
}

class NumberImpl(value: Either[Long,Double]) extends BasicValue(value, Number) {
  
  // TODO: remove Double for ease of use?
  
  override val representation: Any = value match {
    case Left(long) => long
    case Right(double) => double
  }
  
  def canEqual(other: Any): Boolean = other.isInstanceOf[NumberImpl]
  
  override def toString(): String = representation.toString()
  
  override def reprGreaterThan(value: ConcreteValue): Boolean = value.representation match {
    case long1: Long => this.representation match {
      case long2: Long => long1 < long2
      case double2: Double => long1 < double2
    }
    case double1: Double => this.representation match {
      case long2: Long => double1 < long2
      case double2: Double => double1 < double2
    }
  }
  
  override def add(value: ConcreteValue): ConcreteValue = 
    try {
      val left = this.value.asInstanceOf[Either[Long,Double]]
      val right = value.representation
      
      left.fold(
          long => right match {
            case l: Long => long + l
            case r: Double => long + r
          },
          double => right match {
            case l: Long => double + l
            case r: Double => double + r
          })
    } catch {
      case e: ClassCastException => throw new UnsupportedOperationException
    }
    
  override def subtract(value: ConcreteValue): ConcreteValue = 
    try {
      val left = this.value.asInstanceOf[Either[Long,Double]]
      val right = value.representation
      
      left.fold(
          long => right match {
            case l: Long => long - l
            case r: Double => long - r
          },
          double => right match {
            case l: Long => double - l
            case r: Double => double - r
          })
    } catch {
      case e: ClassCastException => throw new UnsupportedOperationException
    }
  
  override def multiply(value: ConcreteValue): ConcreteValue = 
    try {
      val left = this.value.asInstanceOf[Either[Long,Double]]
      val right = value.representation
      
      left.fold(
          long => right match {
            case l: Long => long * l
            case r: Double => long * r
          },
          double => right match {
            case l: Long => double * l
            case r: Double => double * r
          })
    } catch {
      case e: ClassCastException => throw new UnsupportedOperationException
    }
    
  override def divide(value: ConcreteValue): ConcreteValue = 
    try {
      val left = this.value.asInstanceOf[Either[Long,Double]]
      val right = value.representation
      
      left.fold(
          long => right match {
            case l: Long => long / l.asInstanceOf[Double]
            case r: Double => long / r
          },
          double => right match {
            case l: Long => double / l
            case r: Double => double / r
          })
    } catch {
      case e: ClassCastException => throw new UnsupportedOperationException
    }
    
  override def abs(): ConcreteValue = value match {
    case Left(long) => Math.abs(long)
    case Right(double) => Math.abs(double)
  }
}

class BoolImpl(val value: Boolean) extends BasicValue(value, Bool) {
  
  def canEqual(other: Any): Boolean = other.isInstanceOf[BoolImpl]
  
  override def reprGreaterThan(value: ConcreteValue): Boolean = this.value > value.representation.asInstanceOf[Boolean]
}

class StringImpl(value: String) extends BasicValue (value, String) {
  
  def canEqual(other: Any): Boolean = other.isInstanceOf[StringImpl]
  
  override def reprGreaterThan(value: ConcreteValue): Boolean = this.value > value.representation.asInstanceOf[String]
  
  override def add(value: ConcreteValue): ConcreteValue =
    try {
      this.value concat value.representation.asInstanceOf[String]
    } catch {
      case e: ClassCastException => throw new UnsupportedOperationException
    }
}




abstract class SeqValue(private val seq: Seq[Any], override val aType: AttributeType) extends ConcreteValue with Equals {
  
  override val isList = true
  
  override val representation = seq
  
  override def toString(): String = seq.toString
  
  def length = seq.length
  
  override def equals(other: Any) = {
    other match {
      case that: SeqValue => canEqual(other) && seq == that.seq
      case _ => false
    }
  }
  
  override def hashCode() = {
    val prime = 41
    prime + seq.##
  } 
  
  override def reprContains(value: ConcreteValue): Boolean = seq contains value.representation
}

class DoubleSeqImpl(seq: Seq[Double]) extends SeqValue(seq, Number) {
  
  def canEqual(other: Any) = other.isInstanceOf[DoubleSeqImpl]
}

class IntSeqImpl(seq: Seq[Int]) extends SeqValue(seq, Number) {
  
  def canEqual(other: Any) = other.isInstanceOf[IntSeqImpl]
}

class LongSeqImpl(seq: Seq[Long]) extends SeqValue(seq, Number) {
  
  def canEqual(other: Any) = other.isInstanceOf[LongSeqImpl]
}

class BoolSeqImpl(seq: Seq[Boolean]) extends SeqValue(seq, Bool) {
  
  def canEqual(other: Any) = other.isInstanceOf[BoolSeqImpl]
}

class StringSeqImpl(seq: Seq[String]) extends SeqValue(seq, String) {
  
  def canEqual(other: Any) = other.isInstanceOf[StringSeqImpl]
}

class TimeSeqImpl(seq: Seq[TimeImpl]) extends SeqValue(seq, Time) {
  
  def canEqual(other: Any) = other.isInstanceOf[TimeSeqImpl]
}

class DaySeqImpl(seq: Seq[DayImpl]) extends SeqValue(seq, Day) {
  
  def canEqual(other: Any) = other.isInstanceOf[DaySeqImpl]
}

// FIXME why do the date sequences wrap other Impl classes instead of sequences
// of joda classes? The other SeqImpl classes do wrap raw types.

class DateTimeSeqImpl(seq: Seq[DateTimeImpl]) extends SeqValue(seq, DateTime) {
  
  def canEqual(other: Any) = other.isInstanceOf[DateTimeSeqImpl]
}

class DateTimeDurSeqImpl(seq: Seq[DateTimeDurationImpl]) extends SeqValue(seq, DateTimeDuration) {
  
  def canEqual(other: Any) = other.isInstanceOf[DateTimeDurSeqImpl]
}

class DayDurSeqImpl(seq: Seq[DayDurationImpl]) extends SeqValue(seq, DayDuration) {
  
  def canEqual(other: Any) = other.isInstanceOf[DayDurSeqImpl]
}

class TimeDurSeqImpl(seq: Seq[TimeDurationImpl]) extends SeqValue(seq, TimeDuration) {
  
  def canEqual(other: Any) = other.isInstanceOf[TimeDurSeqImpl]
}