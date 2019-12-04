package playground

import org.scalatest._
import org.sireum.{C, ISZ, Map, Set, Z}
import playground.Alarm._
import playground.RuntimeUtils.{MapUtil, SetUtil}
import playground.WorldCup.Team

class SetUtilTest extends FunSuite{


  val qualificationSet1 = SetUtil.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech))
  val qualificationSet2 = SetUtil.CreateSetFromSeq(ISZ(Qualification.Elec, Qualification.Chem))


  test("Union") {
    val union = SetUtil.SetUnion(qualificationSet1, qualificationSet2)
    assert(union.size == 4)
    assert(union.contains(Qualification.Chem))
    assert(union.contains(Qualification.Elec))
    assert(union.contains(Qualification.Mech))
    assert(union.contains(Qualification.Bio))
  }

  test("Intersection") {
    val intersect = SetUtil.SetIntersect(qualificationSet1, qualificationSet2)
    assert(intersect.size == 1)
    assert(intersect.contains(Qualification.Chem))
  }

  test("Subset 1") {
    val isSubset = SetUtil.Subset(qualificationSet1, qualificationSet2)
    assert(!isSubset)
  }

  test("Subset 2") {
    val s1_subSet = SetUtil.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech))
    val isSubset = SetUtil.Subset(qualificationSet1, s1_subSet)
    assert(isSubset)
    val isPSubset = SetUtil.PSubset(qualificationSet1, s1_subSet)
    assert(!isPSubset)
  }


  //Test from lang-manual
  val s1 : Set[Team.Type] = SetUtil.CreateSetFromSeq(ISZ(Team.France, Team.Denmark, Team.SaudiArabia, Team.SouthAfrica))
  val s2 : Set[Z] = SetUtil.CreateSetFromSeq(ISZ(2,4,6,8,11))
  val s3 : Set[Z] = Set.empty

  test("Union lang"){
    val union = SetUtil.SetUnion(s2, s3)
    assert(union.size == 5)
    assert(union.contains(2))
    assert(union.contains(4))
    assert(union.contains(6))
    assert(union.contains(8))
    assert(union.contains(11))
  }

  test("Intersect lang"){
    val union = SetUtil.SetIntersect(s2, s3)
    assert(union.size == 0)
  }

  test("ProperSubset"){
    assert(!SetUtil.PSubset(s2, s2))
    val s = Set.empty ++ ISZ(1,2,3,4,5,6,7,8,9,10).filter(x => x % 2 == 0).map(x => x * x)
    println(s)
  }



  /*test("Powerset"){
    val set : Set[Z] = SetUtil.CreateSetFromSeq(ISZ(1,2,3))
    val powerset = SetUtil.PowerSet(set)
    assert(powerset.size == 9)
  }

  test("DUnion"){
    val vSet1 : Set[Z]= SetUtil.CreateSetFromSeq(ISZ(2,4))
    val vSet2 : Set[Z] = SetUtil.CreateSetFromSeq(ISZ(4,5,6))
    val vSet3 : Set[Z]= SetUtil.CreateSetFromSeq(ISZ(0,12))

    val setOfSet : Set[Set[Z]] = SetUtil.CreateSetFromSeq(ISZ(SetUtil.CreateSetFromSeq(ISZ(s2, vSet1, vSet2, vSet3))))
    val resultSet = SetUtil.SetDUnion(setOfSet)

    assert(resultSet.size == 8)
    assert(resultSet.contains(0))
    assert(resultSet.contains(2))
    assert(resultSet.contains(4))
    assert(resultSet.contains(5))
    assert(resultSet.contains(6))
    assert(resultSet.contains(8))
    assert(resultSet.contains(11))
    assert(resultSet.contains(12))

  }

  test("DInter"){
    val vSet1 : Set[Z]= SetUtil.CreateSetFromSeq(ISZ(2,4))
    val vSet2 : Set[Z] = SetUtil.CreateSetFromSeq(ISZ(4,5,6))

    val setOfSet : Set[Set[Z]] = SetUtil.CreateSetFromSeq(ISZ(SetUtil.CreateSetFromSeq(ISZ(s2, vSet1, vSet2))))
    val resultSet = SetUtil.SetDIntersection(setOfSet)

    assert(resultSet.size == 1)
    assert(resultSet.contains(4))

  }
*/
}
