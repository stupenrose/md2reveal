package us.penrose.md2revealjs

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
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
   
   test("deepCopy with refs"){
    // given
    val listItem = new ListItem()
    val par = new Paragraph()
    val text = new Text("world")
    val node = compose(new BulletList(),
                  compose(listItem, 
                        compose(new Paragraph(), new Text("hello"))),
                  compose(new ListItem(), 
                        compose(par, text)));
    
    // when
    val (copy, refmap) = CommonmarkUtil.deepCopy(node, List(node, listItem, par, text))
    
    // then
    
    assert(CommonmarkUtil.toString(copy) == CommonmarkUtil.toString(node))
    assert(CommonmarkUtil.toString(listItem) == CommonmarkUtil.toString(refmap(listItem)))
    assert(CommonmarkUtil.toString(par) == CommonmarkUtil.toString(refmap(par)))
    assert(CommonmarkUtil.toString(text) == CommonmarkUtil.toString(refmap(text)))
    assert(CommonmarkUtil.toString(node) == CommonmarkUtil.toString(refmap(node)))
  }
}