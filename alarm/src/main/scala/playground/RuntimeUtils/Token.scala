// #Sireum

package playground.RuntimeUtils

import org.sireum._

@record class Token[T](val value: T) {
  def equalsVDM[Q](obj: Token[Q]): B = {
    return obj.value == value
  }

  def getVDMValue(): String = {
    return value.toString
  }

  def uniqueCode(): Z = {
    if (value != None()) {
      return 0;
    } else {
      return value.hash;
    }
  }

  def toVDMString():String=
  {
    return "mk_token(" + value.toString + ")";
  }
}
