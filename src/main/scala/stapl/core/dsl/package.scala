package stapl.core

package object dsl {
  
  implicit def obligationAction2ObligationActionWithOn(oa: ObligationAction): ObligationActionWithOn = new ObligationActionWithOn(oa)

}