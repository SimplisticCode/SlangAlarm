// #Sireum

package playground.Enigma

import org.sireum._
import playground.RuntimeUtils.{MapUtil, SetUtil}

@record class Plugboard(pa:Alphabet, pcfg:Map[Z,Z]) extends Configuration{

  {
    alph = pa
    config = MapUtil.MUnion(pcfg, MapUtil.Inverse(pcfg))
  }

  @pure def PlugboardInv(pconfig:Map[Z,Z], palph:Alphabet):B={
    return SetUtil.Subset(MapUtil.Dom(pconfig), palph.GetIndices())
  }

/*  def Plugboard(pa:Alphabet, pcfg:Map[Z,Z]):Plugboard={
    alph = pa
    config = MapUtil.MUnion(pcfg, MapUtil.Inverse(pcfg))
    return this
  }*/
  //pre dom pcfg inter rng pcfg = {} and
  //      PlugboardInv(pcfg, pa);


  override def Substitute(pidx:Z):Z ={
    next.get.Rotate()
    //TODO
    //Configuration.Substitute()
  }

}
