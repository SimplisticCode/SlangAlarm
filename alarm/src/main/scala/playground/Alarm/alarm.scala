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
    return SetUtil.CreateSetFromSeq(for(e <- MapUtil.Dom(plant.Schedule.map).elements.filter(peri=> plant.Schedule.map.get(peri).get.contains(expert))) yield e)
  }
}

@datatype class Plant(val Schedule: Schedule, val alarms: Set[AlarmDescription]){

  def invariant(): B = {
    // need to ensure every period has at least one expert on hand in order to handle each type of alarm

    val unhandledAlarms = alarms.elements.filter(a => //
      // is there any period in which an expert is not on hand to handle the alarm
      Schedule.map.valueSet.elements.filter(
        // is there any period in which a qualified expert is not available to handle the alarm (ie. filtered set is empty)?
        s => s.elements.filter(e => e.quali.contains(a.qualification)).isEmpty
      ).nonEmpty
    )

    if(unhandledAlarms.nonEmpty) {
      for (u <- unhandledAlarms.elements) {
        println(s"Alarm ${u} is unhandled")
      }
      println(s"Schedule = ${Schedule}")
      println(s"Alarms = ${alarms}")
    }

    return unhandledAlarms.isEmpty
  }

  {
    if(!invariant()){
      halt("The plant is not valid because not all the schedules contains experts having the necessary qualifications to handle the alarms")
    }
  }
}

@datatype class Schedule(val map: Map[Period, Set[Expert]]){
  def invariant():B = {
    for(sch <- map.entries){
      for(exp1 <- sch._2.elements){
        for(exp2 <- sch._2.elements - exp1){
          if(exp2.expertid == exp1.expertid){
            return F
          }
        }
      }
    }
    return T
  }
  {
    invariant()
  }
}

@datatype class Expert(val expertid: Z, val quali: Set[Qualification.Type])

@datatype class AlarmDescription(val description: String, val qualification: Qualification.Type)

@record class alarm {

  def hello(): Unit = {
    RunTest()
    println("System is running")
  }

  @pure def NumberOfExperts(period: Period, plant: Plant): Z = {
    plant.Schedule.map.get(period) match {
      case Some(experts) => return experts.size
      case _ => return z"0"
    }
  }

    @pure def ExpertToPage(alarm: AlarmDescription, period: Period, plant: Plant): Option[Expert]={
      plant.Schedule.map.get(period) match {
        case Some(experts) =>
          experts.elements.filter((e: Expert) => e.quali.contains(alarm.qualification)) match {
            case ISZ(a) => return Some(a)
            case experts =>
              if(experts.size > 1) {
                halt(s"too many experts with qualification ${alarm.qualification} -- ${experts}")
              }
          }
        case _ =>
      }
      return None[Expert]
    }

    @pure def QualificationOK(exs: Set[Expert], qualification: Qualification.Type): B = {
      return exs.elements.filter(e => e.quali.contains(qualification)).nonEmpty
    }

      def RunTest(): Set[Period]={
        val a1: AlarmDescription = AlarmDescription("Mechanical fault", Qualification.Mech)
        val a2: AlarmDescription = AlarmDescription("Tank overflow", Qualification.Chem)
        val ex1 = Expert(1, Set.empty + Qualification.Mech)
        val ex2 = Expert(2, Set.empty + Qualification.Elec)
        val ex3 = Expert(3, SetUtil.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech)))
        val ex4 = Expert(4,  SetUtil.CreateSetFromSeq(ISZ(Qualification.Elec, Qualification.Chem)))
        val p1 = Period("Monday day")
        val p2 = Period("Monday night")
        val m: Map[Period, Set[Expert]] = Map.empty ++ ISZ(
          (p1, SetUtil.CreateSetFromSeq(ISZ(ex1, ex4))),
          (p2, SetUtil.CreateSetFromSeq(ISZ(ex2, ex3))))
        val plant = Plant(Schedule(m), SetUtil.CreateSetFromSeq(ISZ(a1,a2)))

        return Plant.ExpertIsOnDuty(ex1, plant)
      }
}
