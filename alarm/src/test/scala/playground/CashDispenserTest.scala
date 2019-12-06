package playground

import CashDispenserPP.{Card, Status, System}
import org.scalatest._
import org.sireum._

class CashDispenserTest extends FunSuite {
  var system: CashDispenserPP.System = System().Initialize()

  test("CashDispenser Test - Transaction with correct pin and amount") {
    val c: Card = system.cards.elements(0).t1
    val t = system.tills.values(0).t1
    val correctPin = 123456
    t.InsertCard(c)
    val cardValidated = t.Validate(correctPin)
    val result = t.MakeWithdrawal(1000)
    assert(cardValidated == Status.PinOk)
    assert(result == T)
  }

  test("CashDispenser Test - Transaction amount too big") {
    val c: Card = system.cards.elements(0).t1
    val t = system.tills.values(0).t1
    val correctPin = 123456
    t.InsertCard(c)
    val cardValidated = t.Validate(correctPin)
    val result = t.MakeWithdrawal(1000000)
    assert(cardValidated == Status.PinOk)
    assert(result == F)
  }

  test("CashDispenser Test - Verify Pin Code") {
    val c: Card = system.cards.elements(0).t1
    val t = system.tills.values(0).t1
    val correctPin = 123456
    for (pin <- ISZ(123456, 123457, 123458, 123459, 123460, 123461, 123462)) {
      t.InsertCard(c)
      val cardValidated = t.Validate(pin)
      val result = t.MakeWithdrawal(1000)
      if (pin.equals(correctPin)) {
        assert(cardValidated == Status.PinOk)
        assert(result == T)
      } else if (!pin.equals(correctPin)) {
        assert(cardValidated == Status.PinNotOk | cardValidated == Status.Retained)
        assert(result == F)
      }
    }
  }


}
