package playground

import org.scalatest._
import org.sireum.{ISZ, Map, Set, Z}
import playground.Alarm._
import playground.RuntimeUtils.{MapUtil, SetUtil}

class SetUtilTest extends FunSuite{


  val s1 = Plant.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech))
  val s2 = Plant.CreateSetFromSeq(ISZ(Qualification.Elec, Qualification.Chem))


  test("Union") {
    val union = SetUtil.SetUnion(s1, s2)
    assert(union.size == 4)
    assert(union.contains(Qualification.Chem))
    assert(union.contains(Qualification.Elec))
    assert(union.contains(Qualification.Mech))
    assert(union.contains(Qualification.Bio))
  }

  test("Intersection") {
    val intersect = SetUtil.SetIntersect(s1, s2)
    assert(intersect.size == 1)
    assert(intersect.contains(Qualification.Chem))
  }

  test("Subset 1") {
    val isSubset = SetUtil.Subset(s1, s2)
    assert(!isSubset)
  }

  test("Subset 2") {
    val s1_subSet = Plant.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech))
    val isSubset = SetUtil.Subset(s1, s1_subSet)
    assert(isSubset)
    val isPSubset = SetUtil.PSubset(s1, s1_subSet)
    assert(!isPSubset)
  }


}
