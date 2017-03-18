package us.penrose.md2revealjs

import org.scalatest.FunSuite

class Test extends FunSuite {
 
   import us.penrose.md2revealjs.Util._
   
  test("foobar"){
    // given
    val items = List("andrew", "simon", "alice")
    
    // when
    val groups = groupSequential(items)(_.head)
    
    // then
    assert(groups == List(
          "a" -> List("andrew"),
          "s" -> List("simon"),
          "a" -> List("alice")))
  }
}