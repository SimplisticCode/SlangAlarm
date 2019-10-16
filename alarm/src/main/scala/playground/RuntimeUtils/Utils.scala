// #Sireum

package playground.RuntimeUtils

import org.sireum._

object Utils {
  def Abs(x: R): R = {
    return Abs(x)
  }

  def Floor(x: R): Z = {
    return Floor(x)
  }

  def Div(x : Z, y : Z):Z={
    return Div(x, y)
  }

  def Rem(x : Z, y : Z):Z={
    return Rem(x, y)
  }

  def Mod(x : Z, y : Z):Z={
    return Mod(x, y)
  }

  def is_real[T](exp: T): B={
    return exp.isInstanceOf[R]
  }

  def is_rat[T](exp: T): B={
    return exp.isInstanceOf[R]
  }

  def is_nat[T](exp: T): B={
    return exp.isInstanceOf[Z] & exp.asInstanceOf[Z] >= 0
  }

  def is_nat1[T](exp: T): B={
    return exp.isInstanceOf[Z] & exp.asInstanceOf[Z] > 0
  }

  def is_int[T](exp: T): B={
    return exp.isInstanceOf[Z]
  }

  def is_char[T](exp: T): B={
    return exp.isInstanceOf[C]
  }

  def is_bool[T](exp: T): B ={
    return exp.isInstanceOf[B]
  }

 /* def is_token[T](exp: T): B ={
    return exp.isInstanceOf[Token]
  }
*/
}
