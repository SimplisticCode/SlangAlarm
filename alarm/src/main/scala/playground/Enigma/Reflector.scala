// #Sireum
/*
package playground.Enigma

import org.sireum._
import playground.RuntimeUtils.{MapUtil, SetUtil}

@record class Reflector extends Configuration{

  @pure def ReflectorInv(pNext:Option[Component], pconfig:Map[Z,Z], palph:Alphabet):B={
    val dom = MapUtil.Dom(pconfig)
    val rng = MapUtil.Range(pconfig)
    return pNext == None() & SetUtil.SetIntersect(dom, rng) == Set.empty & (SetUtil.SetUnion(dom, rng) == palph.GetIndices())
  }

  def Reflector(psp:Z, pa:Alphabet, pcfg:Map[Z,Z]):Reflector={
    alph = pa
    config = Map.empty
    for(i <- MapUtil.Dom(pcfg).elements){
      config = config + (pa.Shift(i, psp-1) ~> pa.Shift(pcfg.get(i).get, psp-1))
    }
    return this
  }
  //pre psp in set pa.GetIndices() and
  //  ReflectorInv(next, pcfg, pa);


  override def Substitute(pidx:Z):Z ={
    if(SetUtil.InSet(pidx, MapUtil.Dom(config))){
      return Encode(pidx)
    }else{
      return Decode(pidx)
    }
  }

}
*/