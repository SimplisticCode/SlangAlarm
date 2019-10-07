package RuntimeUtils

import org.sireum.{B, ISZ, Set}

public class SetUtil {
  def Subset[T](set1: Set[T], set2: Set[T]):B={
    if(set1.size <= set2.size) return F
    return set1.intersect(set2).size == set2
  }

  def PSubset[T](set1: Set[T], set2: Set[T]):B={
    if(set1.size < set2.size) return F
    return set1.intersect(set2).size == set2
  }

  def SetDifference[T](set: Set[T], setDif: Set[T]):Set[T]={
    return set.--(setDif.elements)
  }

  def SetIntersect[T](set1: Set[T], set2: Set[T]):Set[T]={
   return set1.intersect(set2)
  }

  def SetUnion[T](set1: Set[T], set2: Set[T]):Set[T]={
    return set1.union(set2)
  }

  def InSet[T](element: T, set: Set[T]):B={
    return set.contains(element)
  }

  def PowerSet[T](set: Set[T]):Set[T]={
    set.
  }

  def CreateSetFromSeq[T](i:ISZ[T]):Set[T]={
    return Set.empty ++ i

  }
}
