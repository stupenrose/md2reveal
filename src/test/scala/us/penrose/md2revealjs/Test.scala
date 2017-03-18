package us.penrose.md2revealjs

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {
 
   import us.penrose.md2revealjs.Util._
   
  test("nonsequential"){
    // given
    val items = List("andrew", "simon", "alice")
    
    // when
    val groups = groupSequential(items)(_.head)
    
    // then
    
    assert(groups == List(
          'a' -> List("andrew"),
          's' -> List("simon"),
          'a' -> List("alice")))
  }
   
   test("sequential"){
    // given
    val items = List("andrew", "alice", "simon")
    
    // when
    val groups = groupSequential(items)(_.head)
    
    // then
    
    assert(groups == List(
          'a' -> List("andrew", "alice"),
          's' -> List("simon")))
  }
}