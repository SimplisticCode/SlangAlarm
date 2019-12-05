// #Sireum

package playground.CashDispenserPP
import org.sireum._;

@record
class WithDrawal extends Event {
  var tillid: Z = 0
  var amount : Z = 0

  def Init(tid: Z, a : Z): WithDrawal = {
    tillid = tid
    amount = a
    return this
  }

  def execute(sys: System): B = {
    val till: Till = sys.GetTill(tillid)
    if (till.CardValidated()) {
      till.MakeWithdrawal(amount)
    } else {
      return F
    }
    return T
  }
}