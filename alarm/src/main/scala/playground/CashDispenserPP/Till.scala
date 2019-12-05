// #Sireum

package playground.CashDispenserPP
import org.sireum._

@enum object Status {
  'PinOk
  'PinNotOk
  'Retained
}

@record class Till(var resource: CentralResource) {
  var curCard: Option[Card] = None()
  var cardOk: B = F
  var retainedCards: Set[Card] = Set.empty;


  //@spec def inc = Invariant(curCard == None() | cardOk)


  def InsertCard(c: Card): Unit = {
    Contract(
      Requires(!CardInside())
    )
    curCard = Some(c)
  }

  @pure def IsLegalCard(): B = {
    Contract(
      Requires(CardInside())
    )
    return resource.IsLegalCard(curCard.get.GetAccountId(), curCard.get.GetCardId())
  }

  @pure def CardInside(): B = {
    return curCard != None()
  }

  @pure def CardValidated(): B = {
    return curCard != None() & cardOk
  }

  def ReturnCard(): Unit = {
    Contract(
      Requires(CardInside())
    )
    cardOk = F
    curCard = None()
  }


  def GetBalance(): Option[Z] = {
    Contract(
      Requires(CardValidated())
    )
    resource.GetBalance(curCard.get.GetAccountId())
  }

  def MakeWithdrawal(amount: Z): B = {
    Contract(
      Requires(CardValidated())
    )
    if(CardValidated()){
      return resource.Withdrawal(curCard.get.GetAccountId(), curCard.get.GetCardId(), amount)
    }else{
      return F
    }
  }

  def RequestStatement(): B = {
    Contract(
      Requires(CardValidated())
    )
    return resource.PostStatement(curCard.get.GetAccountId(), curCard.get.GetCardId())
  }

  @pure def Encode(pin: Z): Z = {
    return pin
  }

  def Validate(pin: Z): Status.Type = {
    Contract(
      Requires(CardInside() & !cardOk)
    )
    val cardId = curCard.get.GetCardId()
    val codeOk = curCard.get.GetCode() == Encode(pin)
    val cardLegal = IsLegalCard()
    cardOk = codeOk & cardLegal

    if (!cardLegal) {
      retainedCards = retainedCards.union((Set.empty ++ ISZ(curCard.get)))
      curCard = None()
      return Status.Retained
    } else if (codeOk) {
      resource.ResetNumberOfTries(cardId)
    } else {
      resource.IncrNumberOfTries(cardId)
      if (resource.NumberOfTriesExceeded(cardId)) {
        retainedCards = retainedCards.union((Set.empty ++ ISZ(curCard.get)))
        cardOk = F
        curCard = None()
        return Status.Retained
      }
    }

    if (cardOk) {
      return Status.PinOk
    } else {
      return Status.PinNotOk
    }
  }
}