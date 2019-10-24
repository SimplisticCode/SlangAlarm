// #Sireum

package playground.Buffer

import org.sireum._

@record class buffers {
  var b1: Z = 0
  var b2: Z = 0
  var b3: Z = 0

  Invariant(
    (b1 + b2 + b3 <= 40) &
      (b2 <= b3) &
      (b3 - b1 <= 15))


  def inv(): Unit = {
    if (!(b1 + b2 + b3 <= 40) & !(b2 <= b3) & (b3 - b1 <= 15)) {
      halt("Variables not valid")
    }
  }

  def Add(x: Z): Unit = {
    Contract(
      Requires(
        x <= 5,
        b1 + b2 + b3 + x <= 40
      ),
      Ensures(
        b1 + b2 + b3 == In(b1) + In(b2) + In(b3) + x
      )
    )
    if (x + b1 < b2) {
      b1 = b1 + x
    } else if (b2 + x <= b3) {
      b2 = b2 + x
    } else {
      b3 = b3 + x
    }
  }

  def Remove(x: Z): Unit = {
    Contract(
      Requires(
        x <= 5,
        b1 + b2 + b3 + x <= 4
      ),
      Ensures(
        b1 + b2 + b3 + x == In(b1) + In(b2) + In(b3)
      )
    )
    if (x + b2 <= b3) {
      b3 = b3 - x
    } else if (x + b1 <= b2) {
      b2 = b2 - x
    } else {
      b1 = b1 - x
    }
  }
}



