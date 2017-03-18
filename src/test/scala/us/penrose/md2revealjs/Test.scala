package us.penrose.md2revealjs

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import us.penrose.md2revealjs.MarkdownToRevealJS.Slide
import org.commonmark.node.Heading
import org.commonmark.node.Paragraph
import org.commonmark.node.BulletList
import org.commonmark.node.ListItem
import org.commonmark.node.Text
import org.commonmark.node.Node
import org.commonmark.parser.Parser

@RunWith(classOf[JUnitRunner])
class Test extends FunSuite {
  import us.penrose.md2revealjs.MarkdownToRevealJS._
  import us.penrose.md2revealjs.CommonmarkUtil._
  
  private def compose(n:Node, children:Node*):Node = {
    children.foreach(n.appendChild(_))
    n
  }
  private def ListItem(n:Node) = {
    val item = new ListItem()
    item.appendChild(n:Node)
    item
  }
  
  private def toString(slide:Slide) = slide.body.map(CommonmarkUtil.toString(_, 0)).mkString("\n")
  
  test("breaks apart nested lists"){
    // given
    
    val document = Parser.builder().build().parse("""
          | - root
          |   - a
          |     - a1
          |   - b
          |     - b1""".stripMargin.trim);
    
    val slide = Slide(new Heading(), streamNodes(document).toList)
    
    println("SLIDE:\n" + toString(slide))
    // when
    val slides = breakApartIfTooLarge(slide, 1)
    
    // then
    
    val List(a, b) = slides
    
    assert(a.h ==  slide.h)
    assert(a.body(0).isInstanceOf[BulletList])
    assert("""
              | BulletList{}
              |  ListItem{}
              |   Text{literal=a}
              |   BulletList{}
              |    Text{literal=a1}""".stripMargin.trim == toString(a).trim)
    assert("""
              | BulletList{}
              |  ListItem{}
              |   Text{literal=b}
              |   BulletList{}
              |    Text{literal=b1}""".stripMargin.trim == toString(b).trim)
  }
  test("breaks apart lists"){
    // given
    val itemA = ListItem(new Text("a"))
    val itemB = ListItem(new Text("b"))
    val ul = compose(new BulletList(), itemA, itemB)
        
    val slide = Slide(new Heading(), List(
        new Paragraph(),
        ul))
    
    // when
    val slides = breakApartIfTooLarge(slide, 1)
    
    // then
    assert(slides.size == 3)
    
    val List(first, second, third) = slides
    
    assert(first ==  Slide(slide.h, List(slide.body.head)))
    assert(second.h ==  slide.h)
    assert(second.body(0).isInstanceOf[BulletList])
    second.body match {
      case List(list:BulletList) => {
        assert(list.getFirstChild == itemA)
      }
      case _ => fail("wrong type")
    }
    
    assert(third.h ==  slide.h)
    third.body match {
      case List(list:BulletList) => {
        assert(list.getFirstChild == itemB)
      }
      case _ => fail("wrong type")
    }
  }
  
  test("breaks apart top-level items"){
    // given
    val slide = Slide(new Heading(), List(
        new Paragraph(),
        new Paragraph()))
    
    // when
    val slides = breakApartIfTooLarge(slide, 1)
    
    // then
    assert(slides.size == 2)
    assert(slides == List(
        Slide(slide.h, List(slide.body.head)),
        Slide(slide.h, List(slide.body.last))
        ))
  }
  
  test("doesn't break apart slides that are not larger than the threshold"){
    // given
    val slide = Slide(new Heading(), List(new Paragraph()))
    
    // when
    val slides = breakApartIfTooLarge(slide, 1)
    
    // then
    assert(slides == List(slide))
  }
  
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