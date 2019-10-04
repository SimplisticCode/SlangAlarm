// #Sireum

package Alarm

import org.sireum._

@enum object Qualification {
  'Elec
  'Mech
  'Bio
  'Chem
}

@datatype class Period(val time: String)

@datatype class Plant(val Schedule: Schedule, val alarms: Set[AlarmDescription]){

  def invariant():B ={
    alarms.elements.foreach(a => Schedule.map.entries.foreach(schedule => if(schedule._2.elements.filter(exp => exp.quali.contains(a.qualification)).size == 0) {
      halt("The plant is not valid because not all the schedules contains experts having the necessary qualifications to handle the alarms")
      return  F}))
    return T
  }

  invariant()
}

@datatype class Schedule(val map: Map[Period, Set[Expert]]){
  def invariant():B ={
    //map.entries.foreach(schedule => if(schedule._2.elements..foreach(exp => exp) return  F))
    return T
  }

  invariant()
}

@datatype class Expert(val expertid: Z, val quali: Set[Qualification.Type])

@datatype class AlarmDescription(val description: String, val qualification: Qualification.Type)

@record class alarm {

  def hello(): Unit = {
    RunTest()
    println("System is running")
  }

  @pure def NumberOfExperts(period: Period, plant: Plant): Z = {
    plant.Schedule.map.get(period).get() match {
      case experts: Set[Expert] => experts.size
      case _ => 0
    }
  }

  @pure def ExpertIsOnDuty(expert: Expert, plant: Plant): Set[Period] = {
    val x: ISZ[Period] = plant.Schedule.map.entries.filter(f  => f._2.contains(expert)).map(m => m._1)
    return CreateSetFromSeq(x)
  }


    @pure def ExpertToPage(alarm: AlarmDescription, period: Period, plant: Plant): Option[Expert]={
      plant.Schedule.map.get(period).get.elements.foreach(exp => if(exp.quali.contains(alarm.qualification)) return Some(exp))
      return None[Expert]
    }

    @pure def QualificationOK(exs: Set[Expert], qualification: Qualification.Type): B ={
      exs.elements.foreach(exp => if (exp.quali.contains(qualification)) return T)
      return F
    }

      def RunTest(): Set[Period]={
        val a1: AlarmDescription = AlarmDescription("Mechanical fault", Qualification.Mech)
        val a2: AlarmDescription = AlarmDescription("Tank overflow", Qualification.Chem)
        val ex1 = Expert(1, Set.empty + Qualification.Mech)
        val ex2 = Expert(2, Set.empty + Qualification.Elec)
        val ex3 = Expert(3, CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech)))
        val ex4 = Expert(4,  CreateSetFromSeq(ISZ(Qualification.Elec, Qualification.Chem)))
        val p1 = Period("Monday day")
        val p2 = Period("Monday night")
        val m: Map[Period, Set[Expert]] = Map.empty ++ ISZ((p1,CreateSetFromSeq(ISZ(ex1, ex4))), (p2, CreateSetFromSeq(ISZ(ex2, ex3))))
        val plant = Plant(Schedule(m), CreateSetFromSeq(ISZ(a1,a2)))

        return ExpertIsOnDuty(ex1, plant)
      }


  def CreateSetFromSeq[T](i:ISZ[T]):Set[T]={
    return Set.empty ++ i
  }

}
