// #Sireum

package playground.CashDispenserPP
import org.sireum._;


@record
class IllegalCard extends Event {
  var myCard: Z = 0

  def Init(c: Z): IllegalCard = {
    myCard = c
    return this
  }

  def execute(sys: System): B = {
    sys.GetResource().AddIllegalCard(myCard)
    return T
  }
}
