package RuntimeUtils

import org.sireum.{ISZ, Set, Z}

public class SequenceUtil {
  def Head[T](seq: Seq[T]):T={
    return seq.head
  }

  def Tail[T](seq: Seq[T]):Seq[T]={
    return seq.tail
  }

  def Len[T](seq: Seq[T]):Z={
    return seq.size
  }

  def Elems[T](seq: Seq[T]):Set[T]={
    return Set.empty ++ ISZ(seq)
  }

  def Inds[T](seq: Seq[T]):Seq[Z]={
    return seq.indices.toSeq
  }

  def Concate[T](seq1: Seq[T], seq2:Seq[T]):Seq[T]={
    return seq1 ++ seq2
  }

  def SeqModification[T](seq: Seq[T], map:Map[Z, T]):Seq[T]={
    val dom = map.keySet
    val indices : Seq[Z] = Inds(seq)

    assert(SetUtil.SubSection(dom, indices))

    val intersection = dom.intersect(Set.empty ++ indices)
    var result : Seq[T] = Seq.empty
    dom.foreach(i => if(indices.contains(i)){
      result = result + map.get(i).get
    }else{
      result = result + seq(i)
    })

    return result
  }

  def ConcateDist[T](seq1: Seq[Seq[T]]):Seq[T]={
    var result : Seq[T] = Seq.empty
    seq1.foreach(o => result = result ++ o)
    return result
  }

  def Reverse[T](seq: Seq[T]): Seq[T]={
    return seq.reverse
  }
}
