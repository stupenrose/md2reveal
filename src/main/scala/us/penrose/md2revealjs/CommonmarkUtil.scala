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
      val (copy, refs) = deepCopy(n, Seq())
      copy
    }
    def deepCopy(n:Node, refs:Seq[Node], filter:(Node)=>Boolean = {n=> true}):(Node, Map[Node, Node]) = {
      println("Deep Copy " + n.getClass.getSimpleName)
      val copy = shallowCopy(n)
      
      
      val refmap = scala.collection.mutable.Map[Node, Node]()
      if(refs.contains(n)){
        println("Found reference to " + n)
        refmap.put(n, copy)
      }
    
      streamNodes(n).filter(filter).foreach { child =>  
        val (childCopy, childMap) = deepCopy(child, refs)
        copy.appendChild(childCopy)  
        
        childMap.foreach{case (key, value)=>
          refmap.put(key, value)
        }
      }
      (copy, refmap.toMap)
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
  
  
  def compose(n:Node, children:Node*):Node = {
    children.foreach(n.appendChild(_))
    n
  }
}