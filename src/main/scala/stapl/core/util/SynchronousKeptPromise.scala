package scala.concurrent.impl

import scala.concurrent.ExecutionContext
import scala.util.{ Try, Success, Failure }
import scala.concurrent.duration.Duration
import scala.concurrent.CanAwait
import java.util.concurrent.ExecutionException

/**
 * An implementation of a KeptPromise that executes callbacks, map and flatMap synchronously,
 * i.e., in the current thread. This is useful for policy evaluation since the callbacks are all
 * very low-latency and we want to avoid the overhead of creating and scheduling new runnables.
 * 
 * Most code copied from KeptPromise since we cannot inherit from this class.
 * 
 * NOTE: THIS CODE IS NEVER USED BECAUSE THE PERFORMANCE TESTS EVENTUALLY SHOWED THAT THE
 * OVERHEAD OF CREATING THE NEW RUNNABLES IS NOT STATISTICALLY SIGNIFANT. Still keeping
 * this code in the repo for future reference.
 */
class SynchronousKeptPromise[T](suppliedValue: Try[T]) extends Promise[T] {

  private def resolveTry[T](source: Try[T]): Try[T] = source match {
    case Failure(t) => resolver(t)
    case _ => source
  }

  private def resolver[T](throwable: Throwable): Try[T] = throwable match {
    case t: scala.runtime.NonLocalReturnControl[_] => Success(t.value.asInstanceOf[T])
    case t: scala.util.control.ControlThrowable => Failure(new ExecutionException("Boxed ControlThrowable", t))
    case t: InterruptedException => Failure(new ExecutionException("Boxed InterruptedException", t))
    case e: Error => Failure(new ExecutionException("Boxed Error", e))
    case t => Failure(t)
  }

  val value = Some(resolveTry(suppliedValue))

  override def isCompleted: Boolean = true

  def tryComplete(value: Try[T]): Boolean = false

  def onComplete[U](func: Try[T] => U)(implicit executor: ExecutionContext): Unit = {
    val completedAs = value.get
    // this is the difference: just call the callback synchronously without scheduling
    // it as a separate runnable
    // Note that this method is called by map, onSuccess etc, so this should be enough
    func(completedAs)
  }

  def ready(atMost: Duration)(implicit permit: CanAwait): this.type = this

  def result(atMost: Duration)(implicit permit: CanAwait): T = value.get.get
}