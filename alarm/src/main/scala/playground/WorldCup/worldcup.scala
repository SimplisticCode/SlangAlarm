// #Sireum
package playground.WorldCup

import org.sireum._
import playground.RuntimeUtils.SetUtil
import playground.RuntimeUtils.MapUtil
//http://overturetool.org/download/examples/VDM++/worldcupPP/index.html


@enum object Team {
  'Brazil
  'Norway
  'Morocco
  'Scotland
  'Italy
  'Chile
  'Austria
  'Cameroon
  'France
  'Denmark
  'SouthAfrica
  'SaudiArabia
  'Nigeria
  'Paraguay
  'Spain
  'Bulgaria
  'Holland
  'Mexico
  'Belgium
  'SouthKorea
  'Germany
  'Yugoslavia
  'Iran
  'UnitedStates
  'Rumania
  'England
  'Colombia
  'Tunisia
  'Argentina
  'Croatia
  'Jamaica
  'Japan
}

@enum object GroupName {
  'A
  'B
  'C
  'D
  'E
  'F
  'G
  'H
}

@datatype class Score(team: Team.Type, won: Z, drawn: Z, lost: Z, points: Z) {
  @spec def inv = Invariant(points == 3 * won + drawn)
}

@record class GroupPhase {

  var gps: Map[GroupName.Type, Set[Score]] =
    Map.empty ++ ISZ((GroupName.A, sc_init(SetUtil.CreateSetFromSeq(ISZ(Team.Brazil, Team.Norway, Team.Morocco, Team.Scotland)))),
    (GroupName.B, sc_init(SetUtil.CreateSetFromSeq(ISZ(Team.Italy, Team.Chile, Team.Austria, Team.Cameroon)))),
    (GroupName.C, sc_init(SetUtil.CreateSetFromSeq(ISZ(Team.France, Team.Denmark, Team.SouthAfrica, Team.SaudiArabia)))),
    (GroupName.D,  sc_init(SetUtil.CreateSetFromSeq(ISZ(Team.Nigeria, Team.Paraguay, Team.Spain, Team.Bulgaria)))),
    (GroupName.E,  sc_init(SetUtil.CreateSetFromSeq(ISZ(Team.Holland, Team.Mexico, Team.Belgium, Team.SouthKorea)))),
    (GroupName.F,  sc_init(SetUtil.CreateSetFromSeq(ISZ(Team.Germany, Team.Yugoslavia, Team.Iran, Team.UnitedStates)))),
    (GroupName.G,  sc_init(SetUtil.CreateSetFromSeq(ISZ(Team.Rumania, Team.England, Team.Colombia, Team.Tunisia)))),
    (GroupName.H,  sc_init(SetUtil.CreateSetFromSeq(ISZ(Team.Argentina, Team.Croatia, Team.Jamaica, Team.Japan)))))

 @spec def inv = Invariant(All(gps.values)(gp => gp.size == 4 & All(gp.elements)(sc => sc.won + sc.lost + sc.drawn <= 3)))

  def sc_init (ts: Set[Team.Type]):Set[Score] ={
    return Set.empty ++ ts.elements.map(t=> Score(t,0,0,0,0))
  }

  @pure def clear_winner(scs: Set[Score]): B = {
    return Exists(scs.elements)(sc => All(scs.elements - sc)(sc1 => sc.t1.points > sc1.t1.points))
  }


  @pure def winner_by_more_wins(scs: Set[Score]): B = {
    if(clear_winner(scs)){
      return T
    } else{
      var maxScore: Z = 0
      for (sc <- scs.elements.map(score => score.points)) {
        if (maxScore < sc) {
          maxScore = sc
        }
      }
      val teamsWithSameScore = scs.elements.filter(score => score.points == maxScore).elements
      var wins:Z = 0
      for(team <- teamsWithSameScore){
        if(team.won == wins){
          return F
        }
      }
    }
    return T
  }


  def Win(wt: Team.Type, wl:Team.Type):Unit={
    Contract(
      Requires(
      )
    )
    var groupName : GroupName.Type = GroupName.A
    for(group <- gps.entries){
      for(score <- group._2.elements){
        if(score.team == wt){
          groupName = group._1
        }
      }
    }

    var groupScore = gps.get(groupName).get
    gps = Map.empty ++ gps.entries.filter(o => o._1 != groupName)
    for(team <- groupScore.elements){
      //Update winning team
      if(team.team == wt){
        groupScore = SetUtil.CreateSetFromSeq(groupScore.elements.filter(o => o.team != team.team))
        val winningTeam = Score(team.team, team.won + 1, team.drawn, team.lost, team.points + 3)
        groupScore = groupScore + winningTeam
      }

      //Update losing team
      if(team.team == wl){
        groupScore = SetUtil.CreateSetFromSeq(groupScore.elements.filter(o => o.team != team.team))
        val losingTeam = Score(team.team, team.won, team.drawn, team.lost + 1, team.points)
        groupScore = groupScore + losingTeam
      }
    }

    //Update Groups
    gps = gps + (groupName ~> groupScore)
  }

  def GroupWinner (gp:GroupName.Type):Team.Type= {
    Contract(
      Requires(
        SetUtil.InSet(gp, MapUtil.Dom(gps))
      )
    )
    val scores = gps.get(gp).get
    //Exists(scores.elements)(sc => All(scores.elements - sc)(sc1 => (sc.points > sc1.points) |
    //(sc.points == sc1.points & sc.won > sc1.won)))
    val clear = clear_winner(scores)
    var maxPoints : Z = 0
    for(score <- scores.elements){
      if(score.points > maxPoints){
        maxPoints = score.points
      }
    }
    val winners = scores.elements.filter(o => o.points == maxPoints)
    if(winners.size == Z(1)) {
      return winners.elements.head.team
    }
    else{
      var maxWins : Z = 0
      for(t <- winners.elements){
        if(t.won > maxWins){
          maxWins = t.won
        }
      }
      return winners.elements.filter(o => o.won == maxWins).head.team
    }
  }

  def GroupRunnerUp (gp: GroupName.Type):Team.Type ={
    Contract(
      Requires(
        SetUtil.InSet(gp, MapUtil.Dom(gps))
      )
    )
    val groupWinner = GroupWinner(gp)
    val scores = gps.get(gp).get.elements.filter(tm => tm.team != groupWinner)
    var maxPoints : Z = 0
    for(score <- scores.elements){
      if(score.points > maxPoints){
        maxPoints = score.points
      }
    }
    val winners = scores.elements.filter(o => o.points == maxPoints)
    if(Z(winners.size) === 1) {
      return winners.head.team
    }
    else{
      var maxWins : Z = 0
      for(t <- winners){
        if(t.won > maxWins){
          maxWins = t.won
        }
      }
      return winners.filter(o => o.won == maxWins).head.team
    }
  }

  def GroupWinners ():Set[Team.Type] = {
    var winners : Set[Team.Type] = Set.empty
    for(group <- gps.entries){
      winners = winners + GroupWinner(group._1)
    }
    return winners
  }
}

