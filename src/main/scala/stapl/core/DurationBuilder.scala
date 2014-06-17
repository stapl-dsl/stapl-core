package stapl.core

import org.joda.time.Period

class DurationBuilder(val number: Int) extends AnyVal {

  def years = Years(number)
  
  def months = Months(number)
  
  def days = Days(number)
  
  def hours = Hours(number)
  
  def minutes = Minutes(number)
  
  def seconds = Seconds(number)
  
  def millis = Millis(number)
}

object Years {
  def apply(number: Int): Duration = new DayDurationImpl(Period.years(number))
}

object Months {
  def apply(number: Int): Duration = new DayDurationImpl(Period.months(number))
}

object Days {
  def apply(number: Int): Duration = new DayDurationImpl(Period.days(number))
}

object Hours {
  def apply(number: Int): Duration = new TimeDurationImpl(Period.hours(number))
}

object Minutes {
  def apply(number: Int): Duration = new TimeDurationImpl(Period.minutes(number))
}

object Seconds {
  def apply(number: Int): Duration = new TimeDurationImpl(Period.seconds(number))
}

object Millis {
  def apply(number: Int): Duration = new TimeDurationImpl(Period.millis(number))
}