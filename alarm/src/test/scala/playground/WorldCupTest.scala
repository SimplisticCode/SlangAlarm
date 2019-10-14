package playground


import org.scalatest._
import org.sireum.{ISZ, Map, Set}
import playground.RuntimeUtils.SetUtil
import playground.WorldCup.{GroupName, GroupPhase, Team}

class WorldCupTest extends FunSuite{
  val gp : GroupPhase = GroupPhase()


  test("Win test"){
    val t1 : Team.Type = Team.Argentina
    val t2 : Team.Type = Team.Croatia
    val t3 : Team.Type = Team.Jamaica
    val t4 : Team.Type = Team.Japan

    gp.Win(t1, t4)
    gp.Win(t2, t3)
    gp.Win(t2, t4)
    gp.Win(t1, t3)
    gp.Win(t1, t2)
    gp.Win(t3, t4)

    val groupWinner = gp.GroupWinner(GroupName.H)
    val runnerUp = gp.GroupRunnerUp(GroupName.H)
    assert(t1 == groupWinner)
    assert(t2 == runnerUp)
  }


}
