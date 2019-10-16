// #Sireum
/*
package playground.Enigma

import org.sireum._
import playground.RuntimeUtils.SetUtil

@msig trait Component {
  def next():Option[Component]
  def alph():Alphabet

  @pure def Successors(): Set[Component] = {
    next match {
      case none: None[Component] => return Set.empty + this
      case comp: Some[Component] => return SetUtil.SetUnion(Set.empty + this, comp.get.Successors())
    }
  }

  def SetNext(component: Component): Unit = {
    assert(next == None())
    assert(!SetUtil.InSet(this, component.Successors()))
    //Send Robby a mail about this
    next = Some(component)
  }

  //   pre next = nil and
  //    self not in set pcom.Successors();


  def Substitute(x: Z): Z

  def Rotate(): Unit

  def Rotate(x: Z): Unit 

}
*/