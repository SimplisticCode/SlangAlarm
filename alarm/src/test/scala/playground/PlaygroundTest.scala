
import org.scalatest._
import org.sireum.{ISZ, Map, Set}
import playground.Alarm.{AlarmDescription, Expert, Period, Plant, Qualification, Schedule}
import playground.RuntimeUtils.SetUtil

class PlaygroundTest extends FunSuite{
  val a1: AlarmDescription = AlarmDescription("Mechanical fault", Qualification.Mech)
  val a2: AlarmDescription = AlarmDescription("Tank overflow", Qualification.Chem)

  val ex1 = Expert(1, Set.empty + Qualification.Mech)
  val ex2 = Expert(2, Set.empty + Qualification.Elec)
  val ex3 = Expert(3, SetUtil.CreateSetFromSeq(ISZ(Qualification.Chem, Qualification.Bio, Qualification.Mech)))
  val ex4 = Expert(4,  SetUtil.CreateSetFromSeq(ISZ(Qualification.Elec, Qualification.Chem)))

  val p1 = Period("Monday day")
  val p2 = Period("Monday night")


  test("Valid plant") {
    val m: Map[Period, Set[Expert]] = Map.empty ++ ISZ(
      (p1, SetUtil.CreateSetFromSeq(ISZ(ex1, ex4))),
      (p2, SetUtil.CreateSetFromSeq(ISZ(ex2, ex3))))

    val plant = Plant(Schedule(m), SetUtil.CreateSetFromSeq(ISZ(a1,a2)))

    // when is expert on duty?
    val expertsSchedule = Plant.ExpertIsOnDuty(ex1, plant)
    println(s"${ex1} is on duty ${expertsSchedule}")
  }

  test("Invalid plant"){
    val m: Map[Period, Set[Expert]] = Map.empty ++ ISZ(
      (p1, SetUtil.CreateSetFromSeq(ISZ(ex4))),
      (p2, SetUtil.CreateSetFromSeq(ISZ(ex2, ex3))))

    val plant = Plant(Schedule(m), SetUtil.CreateSetFromSeq(ISZ(a1, a2)))
    // when is expert on duty?
    val expertsSchedule = Plant.ExpertIsOnDuty(ex1, plant)
    println(s"${ex1} is on duty ${expertsSchedule}")
  }


}
