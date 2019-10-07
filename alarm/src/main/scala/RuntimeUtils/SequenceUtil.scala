package RuntimeUtils

import org.sireum.{ISZ, Set}

class SequenceUtil {
  def Subset[T](set: Set[T], predicate: ):Set[T]={

  }

  def SetDifference[T](set: Set[T], setDif: Set[T]):Set[T]={
    return set.--(setDif.elements)
  }

  def SetIntersect[T](set1: Set[T], set2: Set[T]):Set[T]={
    var intersection : Set[T] = Set.empty
    set1.elements.forEach(s1 => set2.elements.foreach(s2 => if(s1 == s2) intersection + s1))
    return intersection
  }

  def SetUnion[T](set1: Set[T], set2: Set[T]):Set[T]={
    var union : Set[T] = Set.empty
    union ++ set1++ set2
    return union
  }

  def CreateSetFromSeq[T](i:ISZ[T]):Set[T]={
    return Set.empty ++ i
  }
}
