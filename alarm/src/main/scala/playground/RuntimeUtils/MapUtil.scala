// #Sireum
package playground.RuntimeUtils

import org.sireum._

object MapUtil {
  def Dom[A, B](map: Map[A, B]): Set[A] = {
    return map.keySet
  }

  def Inverse[A, B](map: Map[A, B]): Map[B, A] = {
    halt("to do")
  }
  /*
    val dom = Dom(map)
    var resultMap: Map[B, A] = Map.empty
    dom.elements.foreach(v => if (map.get(v)) {
      resultMap = resultMap + (map.get(v), v)
    })
    return resultMap
    */


  def Range[A, B](map: Map[A, B]): Set[B] = {
    return map.valueSet
  }

  def DomRestrictedBy[A, B](map: Map[A, B], set: Set[A]): Map[A, B] = {
    val dom : Set[A] = Set.empty ++ Dom(map).elements.filter(d => !set.contains(d))

    return Map.empty ++ map.entries.filter(e => dom.contains(e._1))
  }

  def DomRestrictedTo[A, B](map: Map[A, B], set: Set[A]): Map[A, B] = {
    halt("to do ")
    //return map.filterKeys(dom => set.contains(dom))
  }

  def RangeRestrictedBy[A, B](map: Map[A, B], set: Set[B]): Map[A, B] = {
    halt("to do ")
    //return map.filter(v => !set.contains(v._2))
  }

  def RangeRestrictedTo[A, B](map: Map[A, B], set: Set[A]): Map[A, B] = {
    halt("to do ")
    //return map.filter(v => set.contains(v._2))
  }

  //MapOverride - ask Jason
  def MapOverride[A, B](map1: Map[A, B], map2: Map[A, B]): Map[A, B] = {
    halt("to do ")
    /*
    var resultMap: Map[A, B] = Map.empty
    val dom1 = Dom(map1)
    val dom2 = Dom(map2)
    val sameElements: Set[A] = dom1.intersect(dom2)

    if (sameElements.nonEmpty) {
      //Override map1 range values with values from map2
      sameElements.elements.foreach(o => if (map1.get(o) != map2.get(o)) resultMap = resultMap + (o, map2.get(o)))
      val uniqueMap1 = dom1 -- sameElements.elements
      val uniqueMap2 = dom2 -- sameElements.elements
      if(uniqueMap1.nonEmpty){
        uniqueMap1.elements.foreach(e => resultMap = resultMap + (e, map1.get(e)))
      }
      if(uniqueMap2.nonEmpty){
        uniqueMap2.elements.foreach(e => resultMap = resultMap + (e, map2.get(e)))
      }
    } else {
      resultMap = MUnion(map1, map2)
    }

    return resultMap

     */
  }

  def MUnion[A, B](map1: Map[A, B], map2: Map[A, B]): Map[A, B] = {
    halt("to do")
    /*
    val dom1 = Dom(map1)
    val dom2 = Dom(map2)
    var sameElements: Set[A] = Set.empty
    dom1.elements.foreach(e => if (dom2.contains(e)) sameElements = sameElements + e)

    if (sameElements.nonEmpty) {
      //Make sure the same elements are mapping to the same values
      var isCompatible: B = T
      sameElements.elements.foreach(o => if (map1.get(o) != map2.get(o)) isCompatible = F)
      assert(isCompatible, Message("The two maps are not compatible"))
    }
    return map1 + map2

     */
  }

  /*def Merge[A, B](map1: Set[Map[A, B]]): Map[A, B] = {
    var resultMap: Map[A, B] = Map.empty
    map1.elements.foreach(m => resultMap = resultMap + m)
    return resultMap
  }*/

  def Equal[A, B](map1: Map[A, B], map2: Map[A, B]): org.sireum.B = {
    return map1.isEqual(map2)
  }

  def NotEqual[A, B](map1: Map[A, B], map2: Map[A, B]): org.sireum.B = {
    return !map1.isEqual(map2)
  }

  def Compose[A, B, C](map1: Map[B, C], map2: Map[A, B]): Map[A, C] = {
    halt("todo")
    /*
    val domM2 = Dom(map2)
    val rangeM1 = Range(map1)
    //Range of Map1 should be subset of domain of Map2
    assert(SetUtil.Subset(domM2, rangeM1))

    var resultMap: Map[A, C] = Map.empty
    map2.foreach(e => resultMap = resultMap + (e._1, map1.get(e._2)))

    return resultMap

     */
  }
}