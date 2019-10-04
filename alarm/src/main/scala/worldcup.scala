// #Sireum

import org.sireum._
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
  def invariant(): Unit = {
    if (!(points == 3 * won + drawn)) {
      halt("Invariant not satisfied")
    }
  }
}

@record class GroupPhase {

  val gps: Map[GroupName.Type, Set[Score]] = Map.empty ++ ISZ((GroupName.A, CreateSetFromSeq(ISZ(Team.Brazil, Team.Norway, Team.Morocco, Team.Scotland))),
    (GroupName.B, CreateSetFromSeq(ISZ(Team.Italy, Team.Chile, Team.Austria, Team.Cameroon))),
    (GroupName.C, CreateSetFromSeq(ISZ(Team.France, Team.Denmark, Team.SouthAfrica, Team.SaudiArabia))),
    (GroupName.D, CreateSetFromSeq(ISZ(Team.Nigeria, Team.Paraguay, Team.Spain, Team.Bulgaria))),
    (GroupName.E, CreateSetFromSeq(ISZ(Team.Holland, Team.Mexico, Team.Belgium, Team.SouthKorea))),
    (GroupName.F, CreateSetFromSeq(ISZ(Team.Germany, Team.Yugoslavia, Team.Iran, Team.UnitedStates))),
    (GroupName.G, CreateSetFromSeq(ISZ(Team.Rumania, Team.England, Team.Colombia, Team.Tunisia))),
    (GroupName.H, CreateSetFromSeq(ISZ(Team.Argentina, Team.Croatia, Team.Jamaica, Team.Japan))))
  //inv forall gp in set rng gps &
  //      (card gp = 4 and
  //       forall sc in set gp & sc.won + sc.lost + sc.drawn <= 3)


  @pure def clear_winner(scs: Set[Score]): B = {
    var maxScore: Z = 0
    var numberWithSameMaxScore: Z = 0
    for (sc <- scs.elements.map(score => score.points)) {
      if (maxScore == sc) {
        numberWithSameMaxScore = numberWithSameMaxScore + 1
      } else if (maxScore < sc) {
        maxScore = sc
        numberWithSameMaxScore = 1
      }
    }
    return (numberWithSameMaxScore == Z(1))
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
        if(team.won ==wins){
          return F
        }
      }
    }
    return T
  }


  def Win(wt: Team.Type, wl:Team.Type):Unit={
    val group = gps.entries.filter(g => g._2.elements.foreach(t => if(t.team.isEqual(wt)) return T)).elements
    gps = gps -- ISZ(group)
    gps = gps ++ (group(0)._1)
  }

  def CreateSetFromSeq[T](i:ISZ[T]):Set[T]={
    return Set.empty ++ i
  }

}

