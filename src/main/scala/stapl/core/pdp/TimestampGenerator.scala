/*
 * Copyright 2015 Jasper Moeys, iMinds-DistriNet, KU Leuven
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package stapl.core.pdp

import java.util.concurrent.atomic.AtomicLong

/**
 * Class responsible for generating a globally unique timestamp. These timestamps
 * are longs that have an ascending order. Notice that these timestamps do not
 * represent actual time, but logical time. *
 */
trait TimestampGenerator {

  /**
   * Returns a unique timestamp that is guaranteed to be higher than all previously
   * generated timestamps.
   */
  def getTimestamp(): String
}

/**
 * Simple implementation of a timing server.
 * Not a robust implementation for distributed scenarios: it is hard to replace it
 * by another timing server because the count is not known externally.
 */
class SimpleTimestampGenerator extends TimestampGenerator {

  private val counter = new AtomicLong(0)

  /**
   * Simple implementation using an internal counter. This leads to a possible
   * exception in uniqueness in case of long roll-over, but this is assumed to fall
   * outside the expected lifespan of any request that might still be executing system-wide.
   */
  override def getTimestamp(): String = counter.incrementAndGet().toString

}
