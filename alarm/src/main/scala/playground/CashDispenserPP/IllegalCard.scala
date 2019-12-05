// #Sireum

package CashDispenserPP

import org.sireum._;


@record
class IllegalCard extends Event {
  var myCard: Z = 0

  def Init(c: Z): IllegalCard = {
    myCard = c
    return this
  }

  def execute(sys: CashDispenserPP.System): B = {
    sys.GetResource().AddIllegalCard(myCard)
    return T
  }
}
