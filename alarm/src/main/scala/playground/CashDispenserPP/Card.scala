// #Sireum

package CashDispenserPP

import org.sireum._

@record class Card(val Code: Z, val CardId: Z, val AccountId: Z) {
  def GetCode(): Z = {
    return Code
  }

  def GetCardId(): Z = {
    return CardId
  }

  def GetAccountId(): Z = {
    return AccountId
  }
}