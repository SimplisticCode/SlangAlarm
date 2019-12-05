// #Sireum

package CashDispenserPP

import org.sireum._;

@record class ReturnCard extends Event {
  var tillid: Z = 0

  def Init(tid: Z): ReturnCard = {
    tillid = tid
    return this
  }

  def execute(sys: CashDispenserPP.System): B = {
    val till: Till = sys.GetTill(tillid)
    if (till.CardInside()) {
      till.ReturnCard()
    } else {
      return F
    }
    return T
  }
}