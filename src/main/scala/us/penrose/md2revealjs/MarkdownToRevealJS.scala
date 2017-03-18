package us.penrose.md2revealjs

import org.httpobjects.jetty.HttpObjectsJettyHandler
import us.penrose.md2revealjs.CommonmarkUtil._
import java.io.{File => FilesystemPath}
import org.apache.commons.io.FileUtils
import org.commonmark.ext.heading.anchor.HeadingAnchorExtension
import java.util.Arrays
import java.nio.charset.Charset
import org.apache.commons.io.IOUtils
import org.commonmark.node.Document
import org.commonmark.node.Node
import org.commonmark.node.Heading
import org.commonmark.node.Visitor
import org.commonmark.node.Paragraph
import org.commonmark.node.BulletList
import org.commonmark.node.Text
import org.commonmark.node.ListItem

object MarkdownToRevealJS extends App {
  
  def breakApartIfTooLarge(slide:Slide, threshold:Int):List[Slide] = {
    
    import us.penrose.md2revealjs.Util._
   
   
    val max = threshold
    
    def textContent(n:Node) = n match {
      case t:Text => t.getLiteral
      case _ => ""
    }
    
    case class Chunk(path:List[Node])
    
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
    		leadup.last.appendChild(nodeCopy)
        List(Chunk(leadup :+ shallowCopy(node)))
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
    
    
    def doit(n:Node, divisibles:List[Chunk]):Node = {
      n match {
        case orig:BulletList => {
          val copy = shallowCopy(orig)
          
          val tails = divisibles.map(_.path.tail)
          val sections = groupSequential(tails)(_.headOption)
          
          sections.foreach{section=>
            copy.appendChild(
                doit(
                    section._1.getOrElse(new Text("unknown")), 
                    section._2.map(Chunk(_))
                    )
            )
          }
          copy
        }
        case n:Node => n
      }
         
    }
    
    def unflatten(divisibles:List[Chunk]):List[Node] = {
      val sections = groupSequential(divisibles)(_.path.head)
      
      val parts = sections.map{case (root, divisibles)=>
          doit(root, divisibles)
      }
      parts.toList
    }
    
    groups.toList.map{divisibles=>
      Slide(slide.h, divisibles.map(_.path.head))
    }
  }
  
  
  import org.commonmark.parser.Parser
  
  val inputPath = new FilesystemPath(args(0))
  val outputPath = new FilesystemPath(args(1))
  
  val template = IOUtils.toString(getClass.getResourceAsStream("template.html"))
  
//  println("Reading " + inputPath)
  val markdown = FileUtils.readFileToString(inputPath, Charset.forName("ASCII"))
  
  val parser = Parser.builder().build();
  
  val document = parser.parse(markdown);
  
  def reduceHeadings(n:Node){
    n match {
      case h:Heading => h.setLevel(h.getLevel+1)
      case _ => 
    }
    streamNodes(n).foreach(reduceHeadings)
  }
  reduceHeadings(document)
  
  
  
  val nodes = streamNodes(document).toList
  
//  println(nodes.size + " nodes")
  
  case class Slide(h:Heading, body:List[Node])
  
  val slides = nodes.foldLeft(Seq[Slide]()){(slides, n)=> 
    n match {
      case h:Heading => {
        slides :+ Slide(h, List())
      }
      case n:Node => {
        slides.lastOption match {
          case None => slides
          case Some(s) => {
            val all = slides.slice(0, slides.size-1)
            all :+ s.copy(body = s.body :+ n)
          }
        }
      }
    }
  }
  
  
  
  
  val expandedSlides = slides.flatMap(breakApartIfTooLarge(_, 5))
  
  
  
  def nestedNodeCount(n:Node):Int = {
    streamNodes(n).foldLeft(0){(accum, child)=>
      accum + 1 + nestedNodeCount(child)
    }
  }
  
  def textSubContent(n:Node):String = {
    streamNodes(n).map({n=>
          n match {
            case t:Text => t.getLiteral
            case n:Node => ""
          }
        }).mkString("")
  }
  
  val outline = streamNodes(document).flatMap{n=>
    n match {
      case h:Heading => {
        
        val text = textSubContent(h)
        
        h.getLevel match {
          case 1 => List(text)
          case _ => List()
        }
        
      }
      case _:Node => List()
    }
  }
  
  def toHtml(n:Node):String = {
    import org.commonmark.renderer.html.HtmlRenderer
    val renderer = HtmlRenderer.builder()
                    .extensions(Arrays.asList(HeadingAnchorExtension.create())).build();
    
    val html = renderer.render(n); 
    html
  }
  
  
  val slideTemplate = """
				<section id="ID_GOES_HERE">
					<h3>TITLE_GOES_HERE</h3>
					<p>
              BODY_GOES_HERE
					</p>

					<aside class="notes">
						Oh hey, these are some notes. They'll be hidden in your presentation, but you can see them if you open the speaker notes window (hit 's' on your keyboard).
					</aside>
				</section>""".trim
  
  def toSpecialBullets(n:Node):String = {
    n match {
      case ul:BulletList => {
        val bulletsHtml = streamNodes(ul).map({b=> 
          s"""<li class="fragment roll-in">${streamNodes(b).map(toSpecialBullets).mkString("\n")}</li>"""
        }).mkString("\n")
        
        s"""<ul>$bulletsHtml</ul>"""
      }
      case n:Node => toHtml(n)
    }
  }
  
	def toSpecialHtml(n:Node):String = n match {
    case p:Paragraph => s"""<p class="fragment roll-in">${streamNodes(p).map(toHtml).mkString("\n")}</p>"""
    case ul:BulletList => toSpecialBullets(n)
    case n:Node => toHtml(n)
  }
  
  val slidesHtml = expandedSlides.zipWithIndex.map({case (slide, idx)=>
//    println("HEADER:" + toHtml(slide.h))
    val bodyHtml = slide.body.map(toSpecialHtml).mkString("\n")
    
    val id = streamNodes(slide.h).map(toHtml).mkString.filter(_.isLetter) + idx
    
    slideTemplate
      .replaceAllLiterally("ID_GOES_HERE", id)
      .replaceAllLiterally("TITLE_GOES_HERE", toHtml(slide.h))
      .replaceAllLiterally("BODY_GOES_HERE", bodyHtml)
      
  }).mkString
  
  val outlineSlideHtml = outline.map({name=>
//    println("OUTLINE: " + name)
     s"""<li>$name</li>"""
  }).mkString("<section><h1>Outline</h1><ul>", "\n", "</ul></section>")
  
  val html = template
        .replaceAllLiterally("TITLE_GOES_HERE", "howdy")
        .replaceAllLiterally("SLIDES_GO_HERE", outlineSlideHtml + slidesHtml)
        
  FileUtils.writeStringToFile(outputPath, html)
//  println("Wrote to " + outputPath)
  
  def fileExtension(name:String):Option[String] = {
    val idx = name.lastIndexOf('.')
    if(idx == -1){
      None
    }else{
      Some(name.substring(idx, name.length()))
    }
  }
   
  def renderMarkdown(text:String) = {
    import org.commonmark.renderer.html.HtmlRenderer
    import org.commonmark.parser.Parser
    val parser = Parser.builder().build();
    
    val document = parser.parse(text);
    val renderer = HtmlRenderer.builder()
                    .extensions(Arrays.asList(HeadingAnchorExtension.create())).build();
    renderer.render(document); 
  }
}
