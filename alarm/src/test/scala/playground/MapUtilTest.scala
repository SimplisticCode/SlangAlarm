package playground


import org.scalatest._
import org.sireum.{ISZ, Map, Set, Z, enum}
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

  test("Merge"){
    val map = MapUtil.Merge(SetUtil.CreateSetFromSeq(ISZ(map1, map2)))
    assert(map.size == 4)
  }

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

  /*
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
  }*/



  val m1 : Map[Team.Type , Z] = Map.empty ++ ISZ((Team.France, 9), (Team.Denmark, 4), (Team.SouthAfrica,2), (Team.SaudiArabia,1))
  val m2 : Map[Z, Z] = Map.empty ++ ISZ((1, 2), (2, 3), (3,4), (4,1))

  test("Dom Countries"){
    val dom = MapUtil.Dom(m1)
    assert(dom.size == 4)
    assert(dom.contains(Team.SaudiArabia))
    assert(dom.contains(Team.SouthAfrica))
    assert(dom.contains(Team.Denmark))
    assert(dom.contains(Team.France))
  }

  test("Range Points"){
    val dom = MapUtil.Range(m1)
    assert(dom.size == 4)
    assert(dom.contains(1))
    assert(dom.contains(2))
    assert(dom.contains(4))
    assert(dom.contains(9))
  }

  test("MUnion"){
    val mapAdd : Map[Team.Type , Z] = Map.empty + (Team.England ~> 3)
    val result = MapUtil.MUnion(m1, mapAdd)
    assert(result.size == 5)
    assert(result.contains(Team.England))
    assert(result.contains(Team.SaudiArabia))
    assert(result.contains(Team.SouthAfrica))
    assert(result.contains(Team.Denmark))
    assert(result.contains(Team.France))
  }

  test("Merge Countries"){
    val map1 : Map[Team.Type , Z] = Map.empty ++ ISZ((Team.France ~> 9),(Team.Spain ~> 4))
    val map2 : Map[Team.Type , Z] = Map.empty ++ ISZ((Team.France ~> 9), (Team.England ~> 3), (Team.UnitedStates ~> 1))
    val setOfMaps = SetUtil.CreateSetFromSeq(ISZ(map1, map2))
    val result = MapUtil.Merge(setOfMaps)

    assert(result.size == 4)
    assert(result.contains(Team.Spain))
    assert(result.contains(Team.England))
    assert(result.contains(Team.UnitedStates))
    assert(result.contains(Team.France))
  }

  test("Override"){
    val mapAdd : Map[Team.Type , Z] = Map.empty ++ ISZ((Team.France ~> 8), (Team.England ~> 4))
    val result = MapUtil.MapOverride(m1, mapAdd)
    assert(result.size == 5)
    assert(result.contains(Team.England))
    assert(result.contains(Team.SaudiArabia))
    assert(result.contains(Team.SouthAfrica))
    assert(result.contains(Team.Denmark))
    assert(result.contains(Team.France))

    assert(result.get(Team.France).get == 8)
  }

  test("Inverse Map"){
    val inversedMap = MapUtil.Inverse(m2)
    val domInv = MapUtil.Dom(inversedMap)
    val rangeInv = MapUtil.Range(inversedMap)
    val domOrg = MapUtil.Dom(m2)
    val rangeOrg = MapUtil.Range(m2)
    assert(domInv.equals(rangeOrg))
    assert(rangeInv.equals(domOrg))

    assert(inversedMap.get(2).get == 1)
    assert(inversedMap.get(3).get == 2)
    assert(inversedMap.get(4).get == 3)
    assert(inversedMap.get(1).get == 4)
  }

  test("Compose"){
    val franceMap: Map[Team.Type, Team.Type] = Map.empty + (Team.France ~> Team.France)
    val composedMap = MapUtil.Compose(m1, franceMap)
    assert(composedMap.size == 1)
    assert(composedMap.contains(Team.France))
    assert(composedMap.get(Team.France).get == 9)
  }

  test("Compose and inverse"){
    val inversed = MapUtil.Inverse(m2)
    val composedMap = MapUtil.Compose(m2, inversed)
    assert(composedMap.size == 4)
    assert(composedMap.get(1).get == 1)
    assert(composedMap.get(2).get == 2)
    assert(composedMap.get(3).get == 3)
    assert(composedMap.get(4).get == 4)
  }
}
