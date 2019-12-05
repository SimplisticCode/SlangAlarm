package playground

import org.scalatest._
import org.sireum._
import playground.RuntimeUtils.SetUtil

class PaceMakerTest extends FunSuite{


  test("Set comprehensions"){
    val s1 = ISZ('a', 'b', 'c')
    val s2: Set[C] = Set.empty ++ (for (j <- s1) yield j)// for comprehension
    println(s2)
  }
/*
  test("Set comprehensions with condition"){
    val alph = ISZ('a', 'b', 'c','d', 'e')
    val pch = 'c'
    val s2: Set[Z] = Set.empty ++ (for (i <- alph.filter(o => o == pch).indices) yield i)// for comprehension
    println(s2)
  }
*/

  test("Set comprehensions with condition"){
    val numbers = Set.empty ++ ISZ(1,2,3,4,5)
    val resultSet: Set[Z] = Set.empty ++ (for (i <- numbers.elements.filter(o => o % 2 == 0)) yield i)// for comprehension
    val expectedResultSet: Set[Z] = Set.empty ++ ISZ(2,4)
    assert(SetUtil.Equals(resultSet, expectedResultSet))
    println(resultSet)
  }

  test("Set comprehensions with condition and mapping"){
    val numbers = Set.empty ++ ISZ(1,2,3)
    val resultSet: Set[Z] = Set.empty ++ (numbers.elements.filter(o => o % 2 == 0).map(x => x*2))// for comprehension
    val expectedResultSet: Set[Z] = Set.empty ++ ISZ(4)
    assert(SetUtil.Equals(resultSet, expectedResultSet))
    println(resultSet)
  }
  //Look SetOps - runtime ISZOps

  test("Map comprehensions with condition and mapping"){
    val numbers = Set.empty ++ ISZ(1,2,3,4,5)
    val result  = Map.empty ++ (Set.empty ++ ISZ(1,2,3,4,5)).elements.filter(o => o % 2 == 0).map(i =>(i, i*2))// for comprehension
    val expected = Map.empty ++ ISZ((2,4),(4,8))
    assert(result == expected)
    println(result)
  }

  test("Map comprehensions with condition and mapping 1"){
    val numbers = Set.empty ++ ISZ(1,2,3)
    val result  = Map.empty ++ numbers.elements.filter(o => 1 == 1).map(i => (i, i*i))
    val expected = Map.empty ++ ISZ((1,1),(2,4),(3,9))
    assert(result == expected)
    println(result)
  }

  test("Map comprehensions with condition and mapping 2"){
    val result  = Map.empty ++ (for (x <- SetUtil.CreateSetFromSeq(ISZ(1, 2, 3)).elements.filter(x=>1.equals(1))) yield (x, x * x))
    val expected = Map.empty ++ ISZ((1,1),(2,4),(3,9))
    assert(result == expected)
    println(result)
  }

  def fact(n:Z): Z = {
    val decreaseEntry : Z = n
    var r : Z = 0
    if(n == 0){
      r =  1
    }else{
      val decreaseN : Z = n - 1
      assert(decreaseN < decreaseEntry)
      assert(decreaseEntry > 0)
      r =  n * fact(decreaseN)
    }
    return r
  }

   Set.empty ++ (for(e <- (Set.empty ++ ISZ(1,2,3)).elements.filter(x => x % 2 == 0)) yield e *e)



  test("a"){
      val x: Set[Z] = Set.empty ++ (SetUtil.CreateSetFromSeq(ISZ(1,2,3,4,5,6,7,8,9)).elements.filter(o => o % 2 == 0).map(x => x*x))// for comprehension
    var t = All(Z(0) until 1000)(x =>  x < (x + 1))
    var f = Exists(Z(0) until 1000)(x =>  x > 10)
    println(f)
      println(t)
      println(x)
    }

  //{x+y+z |-> z+y+x | x,y,z in set {1,2,3} & true}

  test("Multiple bindings"){
    val expected : Map[Z,Z] = Map.empty ++ ISZ((3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9))
    val s : ISZ[Z] =ISZ(1,2,3)
    var result : Map[Z,Z] = Map.empty
    for(x <- s){
      for(y <- s){
        for(z <- s) {
          if(T){
            result = result + ((x+y+z) ~> (x+y+z))
          }
        }
      }
    }
    //var rs = SetUtil.SetDUnion(s.map(x => s.map(y => s.map(z => y + z + x))).map(x => SetUtil.CreateSetFromSeq(x)))
    //var resultMap : Map[Z,Z] = Map.empty ++ (for (v <- s.map(x => s.map(y => s.map(z => x + y + z)))) yield (v, v))
    assert(result == expected)
    //assert(resultMap == expected)
    println(result)
  }
}
