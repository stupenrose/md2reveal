package us.penrose.md2revealjs

import org.commonmark.node.Document
import org.commonmark.node.Node
import org.commonmark.node.Heading
import org.commonmark.node.Visitor
import org.commonmark.node.Paragraph
import org.commonmark.node.BulletList
import org.commonmark.node.Text
import org.commonmark.node.ListItem
import us.penrose.md2revealjs.CommonmarkUtil._

object SlideReducer {
  
  def breakApartIfTooLarge(slide:Slide, threshold:Int):List[Slide] = {
    
    import us.penrose.md2revealjs.Util._
   
   
    val max = threshold
    
    def textContent(n:Node) = n match {
      case t:Text => t.getLiteral
      case _ => ""
    }
    
    def depthOf(n:Node, tree:Node, depth:Int = 0):Option[Int] = {
      
      if(n == tree)  Some(depth + 1)
      else{
        val depths = streamNodes(tree).flatMap{child=>
          depthOf(n, child, depth +1)
        }
        depths.headOption
      }
      
    }
    
//    def nodesAtDepth(tree:Node, depth:Int):List[Node] = {
//      
//    }
    def pruneToDepth(tree:Node, depth:Int) {
      val children = streamNodes(tree).toList
      if(depth==0){
        children.foreach(_.unlink())
      }else{
        children.foreach{child=>
          pruneToDepth(child, depth -1)
        }
      }
    }
    
    case class Chunk(path:List[Node], item:Node, insertionPoint:Node){
      def parentTree() = {
        val (copy, map) = deepCopy(path.head, List(), filter={n=> n!=item})
        copy
      }
      
      def oneStepUp = {
        val actualTree = path.head
        val currentDepth = depthOf(insertionPoint, actualTree).get
        val currentTree = parentTree
        pruneToDepth(currentTree, currentDepth -1)
        currentTree
      }
    }
    
    def chunkIntoDivisibles(n:Node):List[Chunk] = {
      flattenTree(List(), None, n)
    }
    
    def firstPath(p:List[Node]):List[Node] = {
      val n = p.last
      val children = streamNodes(n).toList
      if(children.size == 0){
        p
      }else{
        firstPath(p :+ children(0))
      }
    }
    
    
    def flattenTree(leadup:List[Node], insertionPointMaybe:Option[Node], node:Node):List[Chunk] = {
      
      val children = streamNodes(node).toList
      println("\n\n##### FLATTEN TREE ###############")
  		println("Copying a " + node.getClass.getSimpleName)
  		println("Insertion point is " + insertionPointMaybe)
  		leadup match {
        case List() => println("Leadup is empty")
        case _ => {
          println("Leadup edge is a " + leadup.last.getClass.getSimpleName + " (" + leadup.last + ")")
		      println("Leadup is:\n" + CommonmarkUtil.toString(leadup.head, 4))
		      println("Edge is insertionPoint?? " + (leadup.last == insertionPointMaybe.get))
        }
      }
		  
  		val nodeCopy = shallowCopy(node)
      if(children.size==0) {
    		println(node.getClass.getSimpleName + " is the end of the line")
    		val insertionPoint = leadup.last
    		insertionPoint.appendChild(nodeCopy)
        List(Chunk(leadup :+ nodeCopy, nodeCopy, insertionPoint))
      }
      else{
    		children.flatMap{child=>
    		  val leadingSiblings = children.slice(0, children.indexOf(child))
    		  val leadingSimpleSiblings = leadingSiblings.filter{sibling=>
    		    val n = streamNodes(sibling).toList
    		    n.size == 1 && n.head.isInstanceOf[Text]
    		  }
    		  leadingSimpleSiblings.foreach{prefix=>
    		    val prefixCopy = deepCopy(prefix)
    		    println("Adding sibling:  a " + prefix.getClass.getSimpleName + " to a " + nodeCopy.getClass.getSimpleName)
    		    nodeCopy.appendChild(prefixCopy)
    		  }
      		val leadupCopy = if(leadup.isEmpty){
      		  List()
      		}else{
      		  
      		  val (leadupCopyHead, copyMap) = deepCopy(leadup.head, insertionPointMaybe.toList)
      		  println(copyMap + "  " + insertionPointMaybe.toList)
      		  val insertionPointCopy = insertionPointMaybe match {
      		    case Some(insertionPoint) => copyMap(insertionPoint)
      		    case None => leadup.last
      		  }
      		  
      		  val leadupCopy = firstPath(List(leadupCopyHead))
      		  println("Leadup copy is:\n" + CommonmarkUtil.toString(leadupCopy.head, 4))
    		    println("Adding a " + nodeCopy.getClass.getSimpleName + " to a " + insertionPointCopy.getClass.getSimpleName)
      		  insertionPointCopy.appendChild(nodeCopy)
      		  leadupCopy
      		}
      		
    		  flattenTree(leadupCopy :+ nodeCopy, Some(nodeCopy), child)
    		}
      }
    }
    
    val divisionPoints = slide.body.flatMap{n=> 
      chunkIntoDivisibles(n)
    }
    
    println(divisionPoints.size + " division points:")
    divisionPoints.zipWithIndex.foreach{case (chunk, idx)=> println("   " + idx + CommonmarkUtil.toString(chunk.path.head, 0))}
    
    val groups = divisionPoints.grouped(max)
    
    def unflattenChunks(prev:Chunk, chunk:Chunk):List[Chunk] = {
      val a = prev.parentTree
      val b = chunk.parentTree
      val aTxt = CommonmarkUtil.toString(a)
      val bTxt = CommonmarkUtil.toString(b)
      println(aTxt + "\nvs\n" + bTxt)
       if(aTxt == bTxt){
          println("spliced " + chunk.item + " into " + prev.insertionPoint)
          prev.insertionPoint.appendChild(chunk.item)
          List()
       }else{
         List(chunk)
       }
    }
    
    def unflatten(divisibles:List[Chunk]):List[Chunk] = {
      val collapsed = divisibles.foldLeft(List[Chunk]()){(accum, chunk)=>
        accum.lastOption match {
          case None => accum :+ chunk
          case Some(prev) => {
            // TODO: Keep doing this, walking up the tree, until we either splice or reach the root
            accum ::: unflattenChunks(prev, chunk)
          }
        }
      }
      collapsed
    }
    
    groups.toList.map{divisibles=>
      val initNum = divisibles.size
      var n = divisibles
      var x = 0;
      do{
        println("Iteration " + x)
        x = x+1;
        n = unflatten(divisibles)
      }while(n.size!=initNum)
      
      Slide(slide.h, n.map(_.path.head))
    }
  }
  
}