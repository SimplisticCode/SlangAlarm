package playground


import org.scalatest._
import org.sireum.{ISZ, Set, Z}
import playground.RuntimeUtils.SetUtil

class UseBufferTest extends FunSuite{

  val b : buffers = new buffers()



  test("Add to buffer") {
    val set : Set[Z] = SetUtil.CreateSetFromSeq(ISZ(1,2,3,4,5))
    set.elements.foreach(e => {
      b.Add(e)
    })
  }
}
