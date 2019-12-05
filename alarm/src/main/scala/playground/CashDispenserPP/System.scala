// #Sireum

package CashDispenserPP

import org.sireum._
import org.sireum.ops.ISZOps

@record class System() {

  val c1: Card = Card(123456, 1, 1)
  val c2: Card = Card(123457, 2, 2)
  val c3: Card = Card(123458, 3, 3)
  val c4: Card = Card(123459, 4, 4)
  val c5: Card = Card(123460, 5, 5)
  val c6: Card = Card(123461, 6, 5)
  val c7: Card = Card(123462, 7, 5)
  val cards: Set[Card] = Set.empty ++ ISZ(c1, c2, c3, c4, c5, c6, c7)
  val resource: CentralResource = CentralResource("addr","res")
  val tills: Map[Z, Till] = Map.empty ++ ISZ((1, Till(resource)), (2, Till(resource)), (3, Till(resource)))

  var clock: Clock = Clock()
  var letterbox: Letterbox = Letterbox()

  def GetTill(tid: Z): Till = {
    return tills.get(tid).get
  }

  def GetResource(): CentralResource = {
    return resource
  }

  def Initialize():System={
    clock.SetDate("150999")
    val peter = CardHolder().Create("Peter", "Granvej")
    val paul = CardHolder().Create("Paul Mukherjee", "Rugaardsvej 47")
    val sten = CardHolder().Create("Sten Agerholm", "Teisensvej ??")
    val kim = CardHolder().Create("Kim Sunesen", "??")
    val CSK = CardHolder().Create("CSK", "Forskerparken 10A")
    val pglacc1 = Account().Create(Map.empty + (1 ~> peter),5000)
    val saacc1  = Account().Create(Map.empty + (2 ~> sten),0)
    val ksacc1  = Account().Create(Map.empty + (3 ~> kim),9000)
    val pmacc1  = Account().Create(Map.empty + (4 ~> paul),6000)
    val ifacc1  = Account().Create(Map.empty ++ ISZ((5,peter), (6, sten), (7, CSK)),70000)
    val pglid1 = 1
    val said1 = 2
    val ksid1 = 3
    val pmid1 = 4
    val ifid1 = 5
    resource.AddAccount(pglid1, pglacc1)
    resource.AddAccount(said1, saacc1)
    resource.AddAccount(ksid1, ksacc1)
    resource.AddAccount(pmid1, pmacc1)
    resource.AddAccount(ifid1, ifacc1)
    resource.AddLetterBox(clock, Letterbox())
    return this
  }
}





