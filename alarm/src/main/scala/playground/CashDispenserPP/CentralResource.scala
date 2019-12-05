// #Sireum

package CashDispenserPP

import org.sireum._

@record class CentralResource(val Address: String, val Name: String) {
  var accounts: Map[Z, Account] = Map.empty
  var numberOfTries: Map[Z, Z] = Map.empty
  var illegalCards: Set[Z] = Set.empty
  var clock: Clock = Clock()
  var letterbox: Letterbox = Letterbox()

  val maxNumberOfTries: Z = 3
/*
  @spec def inv = Invariant(
    for (acc1 <- accounts.values) {
      for (acc2 <- accounts.values) {
        !acc1.GetCardIds().union(acc2.GetCardIds()).isEmpty()
      }
    })*/

  def AddLetterBox(c: Clock, l: Letterbox): Unit = {
    clock = c
    letterbox = l
  }

  @pure def GetBalance(account: Z): Option[Z] = {
    if (accounts.contains(account)) {
      Some(accounts.get(account).get.GetBalance())
    } else {
      return None()
    }
  }

  @pure def NumberOfTriesExceeded(cardId: Z): B = {
   return numberOfTries.get(cardId).get >= maxNumberOfTries
  }

  @pure def IncrNumberOfTries(cardId: Z): Unit = {
    val entry = numberOfTries.entry(cardId).get
    numberOfTries = numberOfTries - entry
    numberOfTries = numberOfTries + (cardId ~> (entry._2 + 1))
  }

  @pure def IsLegalCard(accountId: Z, cardId: Z): B = {
    return (!illegalCards.contains(cardId)) & (accounts.contains(accountId)) & accounts.get(accountId).get.GetCardIds().contains(cardId)
  }

  def Withdrawal(accountId: Z, cardId: Z, amount: Z): B = {
    if (IsLegalCard(accountId, cardId)) {
      return accounts.get(accountId).get.Withdrawal(cardId, amount, clock.GetDate())
    } else {
      return F
    }
  }

  def PostStatement(accountId: Z, cardId: Z): B = {
    if (IsLegalCard(accountId, cardId)) {
      letterbox.PostStatement(accounts.get(accountId).get.MakeStatement(cardId, clock.GetDate()))
      return T
    } else {
      return F
    }
  }

  def ResetNumberOfTries(cardId: Z): Unit = {
    val entry = numberOfTries.entry(cardId).get
    numberOfTries = numberOfTries - entry
    numberOfTries = numberOfTries + (cardId ~> 0)

  }

  def AddAccount(accId: Z, acc: Account): Unit = {
    Contract(
      Requires(!accounts.contains(accId))
    )
    accounts = accounts + (accId ~> acc)
    numberOfTries = numberOfTries //Todo
  }


  def AddIllegalCard(cid: Z): Unit = {
    illegalCards = illegalCards + cid
  }
}
