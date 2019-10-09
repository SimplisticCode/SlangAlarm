// #Sireum
package playground.RuntimeUtils

import org.sireum._
import org.sireum.message.Message

object MapUtil {
  def Dom[T,K](map: Map[T,K]): Set[T] = {
    return map.keySet
  }

  //Should be one-to-one
  def Inverse[T,K](map: Map[T,K]): Map[K,T] = {
    val dom : Set[T]= Dom(map)
    val range : Set[K] = Range(map)
    assert(dom.size == range.size)
    var resultMap: Map[K, T] = Map.empty
    for(e <- dom.elements){
      resultMap = resultMap + (map.get(e).get ~> e)
    }
    return resultMap
  }


  def Range[T,K](map: Map[T,K]): Set[K] = {
    return map.valueSet
  }

  def DomRestrictedBy[T, K](map: Map[T, K], set: Set[T]): Map[T, K] = {
    val dom : Set[T] = Set.empty ++ Dom(map).elements.filter(d => !set.contains(d))
    return Map.empty ++ map.entries.filter(e => dom.contains(e._1))
  }

  def DomRestrictedTo[T, K](map: Map[T,K], set: Set[T]): Map[T,K] = {
    val dom : Set[T] = Set.empty ++ Dom(map).elements.filter(d => set.contains(d))
    return Map.empty ++ map.entries.filter(e => dom.contains(e._1))
  }

  def RangeRestrictedBy[T,K](map: Map[T,K], set: Set[K]): Map[T,K] = {
    val range : Set[K] = Set.empty ++ Range(map).elements.filter(d => !set.contains(d))
    return Map.empty ++ map.entries.filter(e => range.contains(e._2))
  }

  def RangeRestrictedTo[T,K](map: Map[T,K], set: Set[K]): Map[T,K] = {
    val range : Set[K] = Set.empty ++ Range(map).elements.filter(d => set.contains(d))
    return Map.empty ++ map.entries.filter(e => range.contains(e._2))
  }


  def MapOverride[A, B](map1: Map[A, B], map2: Map[A, B]): Map[A, B] = {

    var resultMap: Map[A, B] = Map.empty
    val dom1 = Dom(map1)
    val dom2 = Dom(map2)
    val sameDomElements: Set[A] = dom1.intersect(dom2)

    if (sameDomElements.nonEmpty) {
      //Override map1 range values with values from map2
      for(elem <- sameDomElements.elements){
        if (map1.get(elem) != map2.get(elem)) {
          resultMap = resultMap + (elem ~> map2.get(elem).get)
        }
      }
      val uniqueMap1 = dom1 -- sameDomElements.elements
      val uniqueMap2 = dom2 -- sameDomElements.elements
      if(uniqueMap1.nonEmpty){
        for(elem <- uniqueMap1.elements){
          resultMap = resultMap + (elem ~> map1.get(elem).get)
        }
      }
      if(uniqueMap2.nonEmpty){
        for(elem <- uniqueMap2.elements){
          resultMap = resultMap + (elem ~> map2.get(elem).get)
        }
      }
    } else {
      resultMap = MUnion(map1, map2)
    }

    return resultMap
  }


  def MUnion[T, K](map1: Map[T,K], map2: Map[T,K]): Map[T,K] = {
    val dom1 = Dom(map1)
    val dom2 = Dom(map2)
    val sameElements: Set[T] = dom1.intersect(dom2)

    if (sameElements.nonEmpty) {
      //Make sure the same elements are mapping to the same values
      var isCompatible : B = T
      for(e <- sameElements.elements){
        if(map1.get(e).get != map2.get(e).get){
          isCompatible = F
        }
      }
      assert(isCompatible)
    }
    return map1 ++ map2.entries
  }

  def Merge[T,K](setOfMaps: Set[Map[T,K]]): Map[T,K] = {
    var resultMap: Map[T, K] = Map.empty
    for(set <- setOfMaps.elements){
      resultMap = resultMap ++ set.entries
    }
    return resultMap
  }

  def Equal[A, B](map1: Map[A, B], map2: Map[A, B]): org.sireum.B = {
    return map1.isEqual(map2)
  }

  def NotEqual[A, B](map1: Map[A, B], map2: Map[A, B]): org.sireum.B = {
    return !map1.isEqual(map2)
  }

  def Compose[T,K,L](map1: Map[K,L], map2: Map[T,K]): Map[T, L] = {
    val domM1 : Set[K]= Dom(map1)
    val rangeM2 : Set[K]= Range(map2)
    //Range of Map2 should be subset of domain of Map1
    assert(SetUtil.Subset(domM1, rangeM2))

    var resultMap: Map[T,L] = Map.empty
    for(e <- map2.entries){
      resultMap = resultMap + (e._1 ~> map1.get(e._2).get)
    }

    return resultMap
  }
}
