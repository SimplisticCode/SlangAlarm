package playground


import org.scalatest._
import org.sireum.{ISZ, Map, Set, Z}
import playground.Alarm._
import playground.RuntimeUtils.{MapUtil, SetUtil}

class MapUtilTest extends FunSuite{

  val ex1 = Expert(1, Set.empty + Qualification.Mech)
  val ex2 = Expert(2, Set.empty + Qualification.Elec)
  val ex3 = Expert(3, SetUtil.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech)))
  val ex4 = Expert(4,  SetUtil.CreateSetFromSeq(ISZ(Qualification.Elec, Qualification.Chem)))

  val map1: Map[Z, Set[Expert]] = Map.empty ++ ISZ(
    (1, SetUtil.CreateSetFromSeq(ISZ(ex1, ex4))),
    (2, SetUtil.CreateSetFromSeq(ISZ(ex2, ex3))))

  val map2: Map[Z, Set[Expert]] = Map.empty ++ ISZ(
    (3, SetUtil.CreateSetFromSeq(ISZ(ex1, ex4))),
    (4, SetUtil.CreateSetFromSeq(ISZ(ex2, ex3))))

  test("Dom") {
    val dom = MapUtil.Dom(map1)
    assert(dom.contains(1))
    assert(dom.contains(2))
    assert(dom.size == 2)
  }

  test("Range"){
    val range = MapUtil.Range(map1)
    assert(range.contains(SetUtil.CreateSetFromSeq(ISZ(ex1, ex4))))
    assert(range.contains(SetUtil.CreateSetFromSeq(ISZ(ex2, ex3))))
    assert(range.size == 2)
  }

 /* test("Merge"){
    val map = MapUtil.Merge(SetUtil.CreateSetFromSeq(ISZ(map1, map2)))
    assert(map.size == 4)
  }*/

  test("Not Equal"){
    //assertResult(false, MapUtil.Equal(map1, map2))
    assert(MapUtil.NotEqual(map1, map2))
  }

  test("DomRestrictedBy"){
    val set = SetUtil.CreateSetFromSeq(ISZ(Z(2),Z(3)))
    val mapRestricted = MapUtil.DomRestrictedBy(map1, set)
    assert(mapRestricted.size == 1)
    assert(mapRestricted.keySet.contains(1))
  }

  test("DomRestrictedTo"){
    val set = SetUtil.CreateSetFromSeq(ISZ(Z(2),Z(3)))
    val mapRestricted = MapUtil.DomRestrictedTo(map1, set)
    assert(mapRestricted.size == 1)
    assert(mapRestricted.keySet.contains(2))
  }

  test("RangeRestrictedBy"){
    val set : Set[Expert]= SetUtil.CreateSetFromSeq(ISZ(ex2, ex4))
    val mapRestricted = MapUtil.RangeRestrictedBy(map1, set)
    assert(mapRestricted.size == 1)
    assert(mapRestricted.valueSet.contains(ex1))
  }

  test("RangeRestrictedTo"){
    val set : Set[Expert] = SetUtil.CreateSetFromSeq(ISZ(ex2, ex4))
    val mapRestricted = MapUtil.RangeRestrictedTo(map1, set)
    assert(mapRestricted.size == 1)
    assert(mapRestricted.valueSet.contains(ex4))
  }


}
