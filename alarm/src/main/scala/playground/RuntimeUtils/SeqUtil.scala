// #Sireum
package playground.RuntimeUtils

import org.sireum._
import org.sireum.ops.ISZOps

object SeqUtil {

  def Head[T](seq: ISZ[T]): T = {
    assert(seq.nonEmpty)
    return ISZOps(seq).first
  }

  def Tail[T](seq: ISZ[T]): ISZ[T] = {
    assert(seq.nonEmpty)
    return ISZOps(seq).tail
  }

  def Len[T](seq: ISZ[T]): Z = {
    return seq.size
  }

  def Elems[T](seq: ISZ[T]): Set[T] = {
    return Set.empty ++ seq
  }

  def Inds[T](seq: ISZ[T]): Set[Z] = {
    return Set.empty ++ seq.indices
  }

  def Concate[T](seq1: ISZ[T], seq2: ISZ[T]): ISZ[T] = {
    return seq1 ++ seq2
  }

  def SeqModification[T](seq: ISZ[T], map: Map[Z,T]): ISZ[T] = {
    val dom = MapUtil.Dom(map)
    val indices = Inds(seq)

    assert(SetUtil.Subset(indices, dom))

    val intersection = dom.intersect(SetUtil.CreateSetFromSeq(indices.elements))
    var result: ISZ[T] = ISZ()
    for(elem <- indices.elements){
      if (intersection.contains(elem)) {
        result = result ++ ISZ(map.get(elem).get)
      } else {
        result = result ++ ISZ(seq(elem))
      }
    }
    return result
  }

  def ConcateDist[T](seq1: ISZ[ISZ[T]]): ISZ[T] = {
    var result: ISZ[T] = ISZ()
    for(elem <- seq1.elements){
      result = result ++ elem
    }
    return result
  }

  def Reverse[T](seq: ISZ[T]): ISZ[T] = {
    assert(seq.nonEmpty)
    return ISZOps(seq).reverse
  }


}
