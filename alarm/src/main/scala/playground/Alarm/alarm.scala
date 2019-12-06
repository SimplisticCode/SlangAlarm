// #Sireum

package playground.Alarm

import org.sireum._
import playground.RuntimeUtils.{MapUtil, SetUtil}

@enum object Qualification {
  'Elec
  'Mech
  'Bio
  'Chem
}

@datatype class Period(val time: String)

object Plant {
  @pure def ExpertIsOnDuty(expert: Expert, plant: Plant): Set[Period] = {
    return SetUtil.CreateSetFromSeq(MapUtil.Dom(plant.Schedule.map).elements.filter(peri => plant.Schedule.map.get(peri).get.contains(expert)))
  }
}

@datatype class Plant(val Schedule: Schedule, val alarms: Set[AlarmDescription]) {
  @pure def QualificationOK(exs: Set[Expert], qualification: Qualification.Type): B = {
    return exs.elements.filter(e => e.quali.contains(qualification)).nonEmpty
  }

  @spec def inv = Invariant(All(alarms.elements)(a => All(Schedule.map.keySet.elements)(period => QualificationOK(Schedule.map.get(period).get, a.qualification))))
}

@datatype class Schedule(val map: Map[Period, Set[Expert]]) {
  @spec def inv = Invariant(All(map.valueSet.elements.filter(exs => exs != Set.empty))(exs => All(exs.elements)(e1 => All(exs.elements)(e2 => e1.expertid != e2.expertid))))
}

@datatype class Expert(val expertid: Z, val quali: Set[Qualification.Type])

@datatype class AlarmDescription(val description: String, val qualification: Qualification.Type)

@record class alarm {
  @pure def NumberOfExperts(period: Period, plant: Plant): Z = {
    Contract(
      Requires(
        SetUtil.InSet(period, MapUtil.Dom(plant.Schedule.map))
      )
    )

    plant.Schedule.map.get(period) match {
      case Some(experts) => return experts.size
      case _ => return z"0"
    }
  }

  @pure def ExpertToPage(alarm: AlarmDescription, period: Period, plant: Plant): Option[Expert] = {
    Contract(
      Requires(
        SetUtil.InSet(period, MapUtil.Dom(plant.Schedule.map)),
        SetUtil.InSet(alarm, plant.alarms)
      ),
      Ensures(
        SetUtil.InSet(Res, plant.Schedule.map.get(period).get),
        SetUtil.InSet(alarm.qualification, Res[Set[Qualification.Type]])
      )
    )

    plant.Schedule.map.get(period) match {
      case Some(experts) =>
        experts.elements.filter((e: Expert) => e.quali.contains(alarm.qualification)) match {
          case ISZ(a) => return Some(a)
          case experts =>
            if (experts.size > 1) {
              halt(s"too many experts with qualification ${alarm.qualification} -- ${experts}")
            }
        }
      case _ =>
    }
    return None[Expert]
  }


  @pure def RunTest(): Set[Period] = {
    val a1: AlarmDescription = AlarmDescription("Mechanical fault", Qualification.Mech)
    val a2: AlarmDescription = AlarmDescription("Tank overflow", Qualification.Chem)
    val ex1 = Expert(1, Set.empty + Qualification.Mech)
    val ex2 = Expert(2, Set.empty + Qualification.Elec)
    val ex3 = Expert(3, SetUtil.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech)))
    val ex4 = Expert(4, SetUtil.CreateSetFromSeq(ISZ(Qualification.Elec, Qualification.Chem)))
    val p1 = Period("Monday day")
    val p2 = Period("Monday night")
    val m: Map[Period, Set[Expert]] = Map.empty ++ ISZ(
      (p1, SetUtil.CreateSetFromSeq(ISZ(ex1, ex4))),
      (p2, SetUtil.CreateSetFromSeq(ISZ(ex2, ex3))))
    val plant = Plant(Schedule(m), SetUtil.CreateSetFromSeq(ISZ(a1, a2)))

    return Plant.ExpertIsOnDuty(ex1, plant)
  }
}
