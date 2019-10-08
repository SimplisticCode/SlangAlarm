package playground


import org.scalatest._
import org.sireum.{ISZ, Map, Set, Z}
import playground.Alarm._
import playground.RuntimeUtils.MapUtil

class MapUtilTest extends FunSuite{

  val ex1 = Expert(1, Set.empty + Qualification.Mech)
  val ex2 = Expert(2, Set.empty + Qualification.Elec)
  val ex3 = Expert(3, Plant.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech)))
  val ex4 = Expert(4,  Plant.CreateSetFromSeq(ISZ(Qualification.Elec, Qualification.Chem)))

  val map1: Map[Z, Set[Expert]] = Map.empty ++ ISZ(
    (1, Plant.CreateSetFromSeq(ISZ(ex1, ex4))),
    (2, Plant.CreateSetFromSeq(ISZ(ex2, ex3))))

  val map2: Map[Z, Set[Expert]] = Map.empty ++ ISZ(
    (3, Plant.CreateSetFromSeq(ISZ(ex1, ex4))),
    (4, Plant.CreateSetFromSeq(ISZ(ex2, ex3))))

  test("Dom") {
    val dom = MapUtil.Dom(map1)
    assert(dom.contains(1))
    assert(dom.contains(2))
    assert(dom.size == 2)
  }

  test("Range"){
    val range = MapUtil.Range(map1)
    assert(range.contains(Plant.CreateSetFromSeq(ISZ(ex1, ex4))))
    assert(range.contains(Plant.CreateSetFromSeq(ISZ(ex2, ex3))))
    assert(range.size == 2)
  }

 /* test("Merge"){
    val map = MapUtil.Merge(Plant.CreateSetFromSeq(ISZ(map1, map2)))
    assert(map.size == 4)
  }*/

  test("Not Equal"){
    //assertResult(false, MapUtil.Equal(map1, map2))
    assert(MapUtil.NotEqual(map1, map2))

  }


}
