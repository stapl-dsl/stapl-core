package stapl.core

trait BasicPolicy extends DelayedInit {

  val subject = stapl.core.subject // FIXME do we work on the single subject object here? we need a local copy of some sort
  val resource = stapl.core.resource
  val action = stapl.core.action
  val env = stapl.core.environment
  
  override def delayedInit(body: => Unit): Unit = {
    val code = (() => body)
    code()
  }

}