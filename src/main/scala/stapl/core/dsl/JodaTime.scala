package stapl.core.dsl

import org.joda.time.Period
import org.joda.time.LocalDateTime
import org.joda.time.LocalDate
import org.joda.time.LocalTime

trait JodaTime {
  implicit class DurationBuilder(private val number: Int) {
  
    def years = Period.years(number)
    
    def months = Period.months(number)
    
    def days = Period.days(number)
    
    def hours = Period.hours(number)
    
    def minutes = Period.minutes(number)
    
    def seconds = Period.seconds(number)
    
    def millis = Period.millis(number)
  }
  
  def DateTime(year: Int, month: Int, day: Int, hours: Int, minutes: Int, seconds: Int, millis: Int) = 
    new LocalDateTime(year, month, day, hours, minutes, seconds, millis)
  def DateTime(year: Int, month: Int, day: Int, hours: Int, minutes: Int, seconds: Int): LocalDateTime = 
    DateTime(year, month, day, hours, minutes, seconds, 0)
  def Day(year: Int, month: Int, day: Int) =
    new LocalDate(year, month, day)
  def Time(hours: Int, minutes: Int, seconds: Int, millis: Int) = 
    new LocalTime(hours, minutes, seconds, millis)
  def Time(hours: Int, minutes: Int, seconds: Int): LocalTime =
    Time(hours, minutes, seconds, 0)
}