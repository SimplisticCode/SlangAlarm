package playground

import org.scalatest._
import org.sireum._
import playground.RuntimeUtils.{SetUtil, Utils}

class ComprehensionTest extends FunSuite{

  test("Seq comprehensions"){
    val s1 = ISZ('a', 'b', 'c')
    val s2: ISZ[(Z, C)] = for (j <- s1.indices) yield (j, s1(j)) // for comprehension
    println(s2)
  }

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
    val result  = Map.empty ++ (for (i <- numbers.elements.filter(o => 1 == 1)) yield (i, i*i))
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

   Set.empty ++ (for(e <- (Set.empty ++ ISZ(1,2,3)).elements.filter(x => x % 2 == 0)) yield e *e)



  test("a"){
      val x : Set[Z] = Set.empty ++ (SetUtil.CreateSetFromSeq(ISZ(2, 3, 6, 4, 1, 10, 23, 24, 63)).elements.filter(x => x < 10 & Utils.Mod(x,2).equals(0)).map(x => x))
      println(x)
    }

  //{x+y+z |-> z+y+x | x,y in set {1,2,3}, z in set {5} & true}


  test("Multiple bindings"){
    val expected : Map[Z,Z] = Map.empty ++ ISZ((7,7),(8,8),(9,9),(10,10),(11,11))
    val s : ISZ[Z] =ISZ(1,2,3)
    var result : Map[Z,Z] = Map.empty
    for(x <- s){
      for(y <- s){
        val z = 5
        if(T){
          result = result + ((x+y+z) ~> (x+y+z))
        }
      }
    }
    assert(result == expected)
    println(result)
  }
}
