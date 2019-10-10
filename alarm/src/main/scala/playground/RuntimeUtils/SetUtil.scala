// #Sireum

package playground.RuntimeUtils

import org.sireum._

object SetUtil {
  def CreateSetFromSeq[T](i: ISZ[T]): Set[T] = {
    return Set.empty ++ i
  }

  def Subset[T](set1: Set[T], set2: Set[T]): B = {
    if (set1.size < set2.size) {
      return F
    }
    return (set1.intersect(set2).size == set2.size)
  }

  def PSubset[T](set1: Set[T], set2: Set[T]): B = {
    if (set1.size <= set2.size) {
      return F
    }
    return set1.intersect(set2).size == set2.size
  }

  def SetDifference[T](set: Set[T], setDif: Set[T]): Set[T] = {
    return set -- (setDif.elements)
  }

  def SetDUnion[T](set: Set[Set[T]]): Set[T] = {
    var resultSet: Set[T] = set.elements(0)
    for (s <- set.elements) {
      resultSet = resultSet.union(s)
    }
    return resultSet
  }

  def SetDIntersection[T](set: Set[Set[T]]): Set[T] = {
    var resultSet: Set[T] = set.elements(0)
    for (s <- set.elements) {
      resultSet = resultSet.intersect(s)
    }
    return resultSet
  }

  def SetIntersect[T](set1: Set[T], set2: Set[T]): Set[T] = {
    return set1.intersect(set2)
  }

  def SetUnion[T](set1: Set[T], set2: Set[T]): Set[T] = {
    return set1.union(set2)
  }

  def InSet[T](element: T, set: Set[T]): B = {
    return set.contains(element)
  }
/*
  def PowerSet[T](set: Set[T]): Set[Set[T]] = {
    def pwr(t: Set[T], ps: Set[Set[T]]): Set[Set[T]] = {
      if (t.isEmpty) {
        return ps
      }
      else {
        val head = t.elements.elements.head
        val tail = Set.empty ++ (t.elements - head)
        val ps1 = ps ++ ps.elements.map(m => m.elements.elements + head)
        return pwr(tail, ps1)
      }
    }
    //return pwr(set, Set(Set.empty[T]))

  }*/
}
