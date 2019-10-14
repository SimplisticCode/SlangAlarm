// #Sireum
/*
package playground.Enigma

import org.sireum._
import org.sireum.ops.ISZOps
import playground.RuntimeUtils.{MapUtil, SeqUtil, SetUtil, Utils}

@record class Rotor extends Configuration {
  var latch_pos: Z = 0
  var latch_lock: B = F

  @pure def RotorInv(platch_pos: Z, pconfig: Map[Z, Z], palph: Alphabet): B = {
    val ainds = palph.GetIndices()
    return (SetUtil.InSet(latch_pos, ainds)) & (MapUtil.Dom(pconfig) == ainds) & (MapUtil.Range(pconfig) == ainds) & (MapUtil.Range(pconfig).union(MapUtil.Dom(pconfig)).size < MapUtil.Dom(pconfig).size)
  }

  def Rotor(psp: Z, plp: Z, pa: Alphabet, pcfg: Map[Z, Z]): Rotor = {
    latch_pos = pa.Shift(plp, psp - 1)
    config = Map.empty
    for (i <- MapUtil.Dom(pcfg).elements) {
      config = config + (pa.Shift(i, psp - 1) ~> pa.Shift(pcfg.get(i).get, psp - 1))
    }
    return this
  }

  override def Rotate(): Unit = {
    //   -- propagate the rotation to the next component
    //     -- and tell it where our latch position is
    next.get.Rotate(latch_pos)
    // update our own latch position and take the
    // alphabet size into account
    if (latch_pos == alph.GetSize()) {
      latch_pos = 0
    } else {
      latch_pos = latch_pos + 1
    }

    // update the transpositioning relation by
    // shifting all indices one position
    val config1 : Map[Z,Z] = config.clone()
    config = Map.empty
    for (i <- MapUtil.Dom(config1).elements) {
      config = config + (alph.Shift(i) ~> alph.Shift(config1.get(i).get))
    }

    // remember the rotation
    latch_lock = T
  }

  override def Rotate(ppos: Z): Unit = {
    //compare the latch position and the lock
    if (ppos == latch_pos & !latch_lock) {
      // perform the actual rotation
      Rotate()
    } else {
      // otherwise reset the lock
      latch_lock = F
    }
  }

  //pre ppos in set alph.GetIndices();
}
*/