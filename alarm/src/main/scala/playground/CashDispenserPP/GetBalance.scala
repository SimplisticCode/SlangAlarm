// #Sireum

package CashDispenserPP

import org.sireum._;


@record
class GetBalance extends Event {
  var tillid: Z = 0


  def Init(tid: Z): GetBalance = {
    tillid = tid
    return this
  }


    def execute(sys: CashDispenserPP.System): B = {
      val till : Till = sys.GetTill(tillid)
      if (till.CardValidated()) {
        till.GetBalance()
        return T
      }
      return F
    }
}
