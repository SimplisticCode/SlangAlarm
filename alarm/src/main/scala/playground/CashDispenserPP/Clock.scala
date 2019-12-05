// #Sireum

package CashDispenserPP
import org.sireum._

@record class Clock() {
  var Date: String = ""

  def SetDate(d: String): Unit = {
    Date = d
  }

  @pure def GetDate(): String = {
    return Date
  }
}
