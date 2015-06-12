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

package stapl.core

import stapl.core.typeclasses.Instances
import stapl.core.syntax.Syntax

package object dsl extends DSL with Instances with Syntax {
  type Subject = stapl.core.Subject
  type Resource = stapl.core.Resource
  type Action = stapl.core.Action
  type Environment = stapl.core.Environment
  
  /**
   * You might want to manually wrap something in a Value. Use with care.
   */
  def Value[T] = stapl.core.Value.apply[T] _
}
