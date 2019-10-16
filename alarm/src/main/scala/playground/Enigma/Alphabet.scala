// #Sireum
/*
package playground.Enigma

import org.sireum._
import org.sireum.ops.ISZOps
import playground.RuntimeUtils.{SeqUtil, SetUtil, Utils}

@record class Alphabet(val alpha: ISZ[C]) {

  {
    AlphabetInv(alpha)
  }

  @pure def AlphabetInv(palph: ISZ[C]): B = {
    return Utils.Mod(SeqUtil.Len(palph), 2) == 0 & SeqUtil.Len(palph) == SeqUtil.Elems(palph).size
  }

  def GetChar(pidx: Z): C = {
    assert(SetUtil.InSet(pidx, SeqUtil.Inds(alpha)))
    return alpha(pidx)
  }

  def GetIndex(pch: C): Z = {
    return ISZOps(alpha).indexOf(pch)
  }

  @pure def GetSize(): Z = {
    return SeqUtil.Len(alpha)
  }

  @pure def GetIndices(): Set[Z] = {
    return SeqUtil.Inds(alpha)
  }

  def Shift(pidx: Z, poffset: Z): Z = {
    if (pidx + poffset > SeqUtil.Len(alpha)) {
      return pidx + poffset - SeqUtil.Len(alpha)
    } else {
      return pidx + poffset
    }
  }

  //pre pidx in set inds alph and
  //poffset <= len alph;

  def Shift(pidx: Z): Z = {
    return Shift(pidx, 1)
  }
}
*/