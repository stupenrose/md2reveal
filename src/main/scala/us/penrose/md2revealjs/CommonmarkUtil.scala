package us.penrose.md2revealjs

import org.commonmark.node._

object CommonmarkUtil {
  
  def print(n:Node, level:Int = 0) {
    println(toString(n, level))
  }
  
  def toString(n:Node, level:Int = 0):String = {
    val indent = (for(n <- 0 to level) yield " ").mkString
    s"""|${indent + n}
        |${streamNodes(n).map(toString(_, level+1)).mkString("")}""".stripMargin
  }
  
  
    def shallowCopy(n:Node):Node = {
      n match {
        case orig:BulletList => {
          val copy = new BulletList
          copy.setBulletMarker(orig.getBulletMarker)
          copy.setTight(orig.isTight())
          copy
        }
        case item:ListItem => new ListItem
        case p:Paragraph => new Paragraph
        case txt:Text => {
          val copy = new Text
          copy.setLiteral(txt.getLiteral)
          copy
        }
      }
    }
    
    def deepCopy(n:Node):Node = {
      val copy = shallowCopy(n)
      streamNodes(n).foreach { child =>  
        copy.appendChild(deepCopy(child))  
      }
      copy
    }
  
  def streamNodes(document:Node):Stream[Node] = {
     
    def next(n:Node):Stream[Node] = {
      n match {
        case null => Stream.Empty
        case node => Stream.cons(node, next(n.getNext))
      }
    }
    next(document.getFirstChild)
  }
}