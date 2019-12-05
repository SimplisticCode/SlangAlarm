// #Sireum

package playground.CashDispenserPP

import org.sireum._
import org.sireum.ops.ISZOps

@datatype class Transaction(date: String, cardId: Z, amount: Z)

@record class Account() {

  val dailyLimit: Z = 2000
  var cards: Map[Z, CardHolder] = Map.empty
  var balance: Z = 0
  var transactions: ISZ[Transaction] = ISZ()


  //def ValidTransaction(transaction: Transaction): B = $

  @spec def transInv = Invariant(TransactionsInvariant(transactions))

  def Create(cs: Map[Z, CardHolder], b: Z): Account = {
    cards = cs
    balance = b
    return this
  }

  @pure def GetBalance(): Z = {
    return balance
  }

  @pure def GetCardIds(): Set[Z] = {
    return cards.keySet
  }

  def Withdrawal(cardId: Z, amount: Z, date: String): B = {
    Contract(
      Requires(cards.contains(cardId))
    )
    val transaction = Transaction(date, cardId, amount)
    if (balance - amount >= 0 & DateTotal(date, (transactions ++ ISZ(transaction))) <= dailyLimit) {
      balance -= amount
      transactions = transactions ++ ISZ(transaction)
      return T
    } else {
      return F
    }
  }

  def MakeStatement(cardId: Z, date: String): Letter = {
    Contract(
      Requires(cards.contains(cardId))
    )
    val card: CardHolder = cards.get(cardId).get
    val nm: String = card.GetName()
    val addr: String = card.GetAddress()
    val letter : Letter = Letter(addr, nm, date, transactions, balance)
    return letter
  }

  def AddCard(cId: Z, ch: CardHolder): Unit = {
    Contract(
      Requires(!cards.keySet.contains(cId))
    )
    cards = cards + (cId ~> ch)
  }

  def RemoveCard(cId: Z): Unit = {
    Contract(
      Requires(cards.keySet.contains(cId))
    )
    //cards = MapUtil.DomRestrictedBy(cards, (Set.empty ++ ISZ(cId)))
    val entryThatSouldBeRemoved = cards.entry(cId).get
    cards = cards - entryThatSouldBeRemoved
  }

  def TransactionsInvariant(ts: ISZ[Transaction]): B = {
    return All(ts)(d => DateTotal(d.date, ts) <= dailyLimit)
  }

  def DateTotal(date: String, ts: ISZ[Transaction]): Z = {
    return Sum(ts.filter(t => t.date == date).map(t => t.amount))
  }

  def Sum(rs: ISZ[Z]): Z = {
    if (rs.isEmpty) {
      return 0
    } else {
      return ISZOps(rs).first + Sum(ISZOps(rs).tail)
    }
  }
}





