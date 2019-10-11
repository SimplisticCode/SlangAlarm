// #Sireum

package playground.Enigma

import org.sireum._
import playground.RuntimeUtils.{MapUtil, SetUtil}

@record class SimpleEnigma extends Component {
  val refcfg: Map[Z, Z] = Map.empty ++ ISZ((1, 3), (2, 4))
  val rotcfg: Map[Z, Z] = Map.empty ++ ISZ((1, 2), (2, 4), (3, 3), (4, 1))
  val pbcfg: Map[Z, Z] = Map.empty ++ ISZ((2, 3))

  def SimpleEnigma(): SimpleEnigma = {
    val cp = Component()
    val a = Alphabet()
    alph =  a.Alphabet(ISZ('A','D','C','D'))


    return this
  }



  def KeyStroke(pch: C): C = {
    val pidx = alph.GetIndex(pch)
    return alph.GetChar(next.get.Substitute(pidx))
  }

}
