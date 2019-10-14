// #Sireum
/*
package playground.Enigma

import org.sireum._
import playground.RuntimeUtils.SetUtil

@record class Component {

  var next: Option[Component] = None()
  var alph: Alphabet = None()

  @pure def Successors(): Set[Component] = {
    next match {
      case none: None[Component] => return Set.empty + this
      case comp: Some[Component] => return SetUtil.SetUnion(Set.empty + this, comp.get.Successors())
    }
  }

  def SetNext(component: Component): Unit = {
    assert(next == None())
    assert(!SetUtil.InSet(this, component.Successors()))
    next = Some(component)
  }

  //   pre next = nil and
  //    self not in set pcom.Successors();


  def Substitute(x: Z): Z = {}

  def Rotate(): Unit = {}

  def Rotate(x: Z): Unit = {}

}
*/