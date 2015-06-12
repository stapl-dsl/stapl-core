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

/**
 * A trait providing utilities for adding modules.
 */
trait Modules[T] {
  
  private var _modules: List[T] = Nil
  
  def modules : List[T] = _modules
  /*def modules_=(modules: List[T]) {
    _modules = modules
  }*/
  
  /**
   * Add a module.
   */
  def addModule(module: T) {
    _modules = module :: _modules
  }
  
  /**
   * Short notation for adding modules.
   */
  def +=(module: T) = addModule(module)
}
