// #Sireum

package CashDispenserPP

import org.sireum._;

@record
class SendStatement extends Event {
  var tillid: Z = 0

  def Init(tid: Z): SendStatement = {
    tillid = tid
    return this
  }

  def execute(sys: CashDispenserPP.System): B = {
    val till: Till = sys.GetTill(tillid)
    if (till.CardValidated()) {
      till.RequestStatement()
    } else {
      return F
    }
    return T
  }
}