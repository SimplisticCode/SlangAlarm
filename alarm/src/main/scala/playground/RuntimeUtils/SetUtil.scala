// #Sireum

package playground.RuntimeUtils

import org.sireum._

object SetUtil {
  def CreateSetFromSeq[T](i:ISZ[T]):Set[T]={
    return Set.empty ++ i
  }

  def Subset[T](set1: Set[T], set2: Set[T]): B = {
    if(set1.size < set2.size) {return F}
    return (set1.intersect(set2).size == set2.size)
  }

  def PSubset[T](set1: Set[T], set2: Set[T]):B={
    if(set1.size <= set2.size) {
      return F
    }
    return set1.intersect(set2).size == set2.size
  }

  def SetDifference[T](set: Set[T], setDif: Set[T]):Set[T]={
    return set --(setDif.elements)
  }

  def SetDUnion[T](set: Set[Set[T]]):Set[T]={
    var resultSet : Set[T] = set.elements(0)
    for(s <- set.elements){
      resultSet = resultSet.union(s)
    }
    return resultSet
  }

  def SetDIntersection[T](set: Set[Set[T]]):Set[T]={
    var resultSet : Set[T] = set.elements(0)
    for(s <- set.elements){
      resultSet = resultSet.intersect(s)
    }
    return resultSet
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

  /*def PowerSet[T](set: Set[T]):Set[T]={
    if(set.isEmpty)
    {
      return set
    }
    else{
      val e : T = set.elements.elements.head
      val t = set - e
      val pt = PowerSet(t)
      var fept = Set.empty
      fept = fept.elements.map(o => fept = fept + pt + e)
      return Set.empty + pt + fept
    }
  }*/

}
