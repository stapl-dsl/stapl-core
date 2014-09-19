package stapl.core.pdp

/**
 * A trait providing utilities for adding modules.
 */
trait Modules[T] {
  
  private var _modules: List[T] = Nil
  
  def modules : List[T] = _modules
  def modules_=(modules: List[T]) {
    _modules = modules
  }
  
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