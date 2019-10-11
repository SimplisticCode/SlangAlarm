// #Sireum

package playground.Enigma

import org.sireum._
import playground.RuntimeUtils.{MapUtil, SetUtil}

@record class Configuration extends Component {
  var config : Map[Z,Z] = Map.empty

  @pure def Encode(penc:Z):Z= {
    if(SetUtil.InSet(penc, MapUtil.Dom(config))){
      return config.get(penc).get
    }else{
      return penc
    }
  }

  //Good example for helper methods in report
  @pure def Decode(pdec: Z):Z={
    val invcfg = MapUtil.Inverse(config)
    if(SetUtil.InSet(pdec, MapUtil.Dom(invcfg))){
      return config.get(pdec).get
    }else{
      return pdec
    }
  }

  override def Substitute(pidx: Z): Z = {
    return Decode(next.get.Substitute(Encode(pidx)))
  }
  //  pre next <> nil

}
