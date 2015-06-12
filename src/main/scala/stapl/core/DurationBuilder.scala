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

import org.joda.time.Period
import scala.language.implicitConversions

class DurationBuilder(val number: Int) extends AnyVal {

  def years = Years(number)
  
  def months = Months(number)
  
  def days = Days(number)
  
  def hours = Hours(number)
  
  def minutes = Minutes(number)
  
  def seconds = Seconds(number)
  
  def millis = Millis(number)
}

object DurationBuilder {
  implicit def int2DurationBuilder(int: Int) = new DurationBuilder(int)
}

object Years {
  def apply(number: Int): Period = Period.years(number)
}

object Months {
  def apply(number: Int): Period = Period.months(number)
}

object Days {
  def apply(number: Int): Period = Period.days(number)
}

object Hours {
  def apply(number: Int): Period = Period.hours(number)
}

object Minutes {
  def apply(number: Int): Period = Period.minutes(number)
}

object Seconds {
  def apply(number: Int): Period = Period.seconds(number)
}

object Millis {
  def apply(number: Int): Period = Period.millis(number)
}