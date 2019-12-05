// #Sireum

package playground.CashDispenserPP
import org.sireum._;

@record
class InsertCard extends Event {
  var myCard: Option[Card] = None()
  var tillid: Z = 0

  def Init(tid:Z, c: Card): InsertCard = {
    myCard = Some(c)
    tillid = tid
    return this
  }

  def execute(sys: System): B = {
    sys.GetTill(tillid).InsertCard(myCard.get)
    return T
  }
}