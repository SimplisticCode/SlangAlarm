package playground

import org.scalatest._
import org.sireum._
import playground.RuntimeUtils.{SetUtil, Token}

class TokenTest extends FunSuite{

  test("Equality") {
    val s = Token(6)
    val t = Token(1)
    assert(s.equalsVDM(t) == F)
    assert(s.equalsVDM(Token(6)) == T)

  }
}
