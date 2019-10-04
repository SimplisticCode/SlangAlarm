// #Sireum

import org.sireum._

@record class buffers {

  def hello(): Unit = {
    println("System is running")
  }

  var b1: Z = 0
  var b2: Z = 0
  var b3: Z = 0

  def inv(): Unit = {
    if (!(b1 + b2 + b3 <= 40) & !(b2 <= b3) & (b3 - b1 <= 15)) {
      halt("Variables not valid")
    }
  }


  def Add(x: Z): Unit = {
    if (x + b1 < b2) {
      b1 = b1 + x
    } else if (b2 + x <= b3) {
      b2 = b2 + x
    } else {
      b3 = b3 + x
    }
  }

  //pre x <= 5 and b1 + b2 + b3 + x <= 40
  //post b1 + b2 + b3 = b1~ + b2~ + b3~ + x;

  def Remove(x: Z): Unit = {
    if (x + b2 <= b3) {
      b3 = b3 - x
    } else if (x + b1 <= b2) {
      b2 = b2 - x

    } else {
      b1 = b1 - x
    }
  }
   //pre x <= 5 and x <= b1 + b2 + b3
}  //post b1 +b2 + b3 + x = b1 ~ +b2 ~ +b3 ~;



/*
clpublic getBuffers: () ==> nat * nat * natass Buffers
  getBuffers() ==
    return mk_(b1,b2,b3)
instance variables

  b1 : nat := 0;
  b2 : nat := 0;
  b3 : nat := 0;

inv b1 + b2 + b3 <= 40 and b1 <= b2 and b2 <= b3 and b3 - b1 <= 15

operations

public Add: nat ==> ()
Add(x) ==
  if x + b1 < b2
  then b1 := b1 + x
  elseif b2 + x <= b3
  then b2 := b2 + x
  else b3 := b3 + x
pre x <= 5 and b1 + b2 + b3 + x <= 40
post b1 + b2 + b3 = b1~ + b2~ + b3~ + x;

public Remove: nat ==> ()
Remove(x) ==
  if x + b2 <= b3
  then b3 := b3 - x
  elseif x + b1 <= b2
  then b2 := b2 - x
  else b1 := b1 - x
pre x <= 5 and x <= b1 + b2 + b3
post b1 + b2 + b3 + x = b1~ + b2~ + b3~;

public getBuffers: () ==> nat * nat * nat
getBuffers() ==
  return mk_(b1,b2,b3)

end Buffers
class UseBuffers

instance variables

  b : Buffers := new Buffers()

traces

S1: let x in set {1,...,5} in b.Add(x); b.getBuffers()

S1': b.Add(1); (b.Add(2) | b.Add(3)); b.Add(4)

S2: b.Add(2); ((let x in set {1,...,5} in b.Add(x)) |
               (let y in set {1,3,5} in b.Remove(y))){1,2}; b.getBuffers()

S3: b.Add(5){7}; ((let x in set {1,...,5} in b.Add(x)) |
                  (let y in set {1,3,5} in b.Remove(y))){1,2}; b.getBuffers()

S4: let x in set {1,...,5} in b.Add(x);
    ((let x in set {1,...,5} in b.Add(x)) |
     (let y in set {1,3,5} in b.Remove(y))){1,3}; b.getBuffers()

end UseBuffers
 */