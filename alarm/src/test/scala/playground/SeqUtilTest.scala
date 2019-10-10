package playground

import org.scalatest._
import org.sireum.{ISZ, Z, Map}
import playground.RuntimeUtils.{SeqUtil, SetUtil}

class SeqUtilTest extends FunSuite {
  val l1 = ISZ(3, 1, 4, 1, 5, 9, 2)
  val l2 = ISZ(2, 7, 1, 8)
  val l3 : ISZ[Team.Type]= ISZ(Team.England, Team.Rumania, Team.Colombia, Team.Tunisia)

  test("Len") {
    assert(SeqUtil.Len(l1) == 7)
    assert(SeqUtil.Len(l2) == 4)
    assert(SeqUtil.Len(l3) == 4)

  }

  test("Hd") {
    assert(SeqUtil.Head(l1) == 3)
    assert(SeqUtil.Head(l2) == 2)
    assert(SeqUtil.Head(l3) == Team.England)
  }

  test("Tail") {
    assert(SeqUtil.Tail(l1) == ISZ(1, 4, 1, 5, 9, 2))
    assert(SeqUtil.Tail(l2) == ISZ(7, 1, 8))
    assert(SeqUtil.Tail(l3) == ISZ(Team.Rumania, Team.Colombia, Team.Tunisia))
  }

  test("Elems") {
    assert(SeqUtil.Elems(l1) == SetUtil.CreateSetFromSeq(ISZ(2, 9, 5, 1, 4, 1, 3)))
    assert(SeqUtil.Elems(l2) == SetUtil.CreateSetFromSeq(ISZ(8, 1, 7, 2)))
    assert(SeqUtil.Elems(l3) == SetUtil.CreateSetFromSeq(ISZ(Team.Tunisia, Team.Colombia, Team.Rumania, Team.England)))
  }

  test("Reverse") {
    assert(SeqUtil.Reverse(l1) == ISZ(2, 9, 5, 1, 4, 1, 3))
    assert(SeqUtil.Reverse(l2) == ISZ(8, 1, 7, 2))
    assert(SeqUtil.Reverse(l3) == ISZ(Team.Tunisia, Team.Colombia, Team.Rumania, Team.England))
  }

  test("Inds") {
    assert(SeqUtil.Inds(l1) == SetUtil.CreateSetFromSeq(ISZ(1, 2, 3, 4, 5, 6, 7)))
    assert(SeqUtil.Inds(l2) == SetUtil.CreateSetFromSeq(ISZ(1, 2, 3, 4)))
    assert(SeqUtil.Inds(l3) == SetUtil.CreateSetFromSeq(ISZ(1, 2, 3, 4)))
  }

  test("SeqManipulation") {
    val map: Map[Z, Team.Type] = Map.empty ++ ISZ((1, Team.Germany), (3, Team.Nigeria))
    assert(SeqUtil.SeqModification(l3, map) == SetUtil.CreateSetFromSeq(ISZ(Team.England, Team.Germany, Team.Colombia, Team.Nigeria)))
  }

  test("Conc"){
    assert(SeqUtil.Concate(l1, l2) == ISZ(3, 1, 4, 1, 5, 9, 2, 2, 7, 1, 8))
  }

  test("Distributed Conc"){
    assert(SeqUtil.ConcateDist(ISZ(l1, l2)) == ISZ(3, 1, 4, 1, 5, 9, 2, 2, 7, 1, 8))
    assert(SeqUtil.ConcateDist(ISZ(l1, l2)) == SeqUtil.Concate(l1,l2))
    assert(SeqUtil.ConcateDist(ISZ(l1, l2, l1)) == ISZ(3, 1, 4, 1, 5, 9, 2, 2, 7, 1, 8, 3, 1, 4, 1, 5, 9, 2))
  }
}
