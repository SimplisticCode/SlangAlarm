// #Sireum

package CashDispenserPP

import org.sireum._

@record class CardHolder() {
  var Address: String = ""
  var Name: String = ""

  def Create(addr: String, n: String){
    Address = addr
    Name = n
    return this
  }

  def GetName(): String = {
    return Name
  }

  def GetAddress(): String = {
    return Address
  }
}
