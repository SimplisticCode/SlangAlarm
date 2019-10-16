package playground

import org.scalatest._
import org.sireum.ops.ISZOps
import org.sireum.{C, ISZ, Set, Z}
import playground.RuntimeUtils.{MapUtil, SeqUtil, SetUtil}

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
    val result  = Map.empty ++ (Set.empty ++ ISZ(1,2,3,4,5)).elements.filter(o => o % 2 == 0).map(i =>(i, i*2)).elements// for comprehension
    val expected = Map.empty ++ ISZ((2,4),(4,8)).elements
    assert(result == expected)
    println(result)
  }

  test("Map comprehensions with condition and mapping 1"){
    val numbers = Set.empty ++ ISZ(1,2,3)
    val result  = Map.empty ++ (for (i <- numbers.elements.filter(o => 1 == 1)) yield (i, i*i)).elements// for comprehension
    val expected = Map.empty ++ ISZ((1,1),(2,4),(3,9)).elements
    assert(result == expected)
    println(result)
  }

  test("Map comprehensions with condition and mapping 2"){
    val result  = Map.empty ++ (for (x <- SetUtil.CreateSetFromSeq(ISZ(1, 2, 3)).elements.filter(x=>1.equals(1))) yield (x, x * x)).elements
    val expected = Map.empty ++ ISZ((1,1),(2,4),(3,9)).elements
    assert(result == expected)
    println(result)
  }

   Set.empty ++ (for(e <- (Set.empty ++ ISZ(1,2,3)).elements.filter(x => x % 2 == 0)) yield e *e)



  test("a"){
    val s : ISZ[Z] =ISZ(4, 5, 6)
      var a : Set[Z] = Set.empty ++ (for (e <- SeqUtil.Elems(s).elements.filter(x => 1.equals(1)).map(x => x)) yield e)

    }
}
