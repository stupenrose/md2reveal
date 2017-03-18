package us.penrose.md2revealjs

import org.httpobjects.jetty.HttpObjectsJettyHandler
import org.httpobjects.{HttpObject, Request}
import org.httpobjects.DSL._
import java.io.{File => FilesystemPath}
import org.apache.commons.io.FileUtils
import org.httpobjects.util.ClasspathResourceObject
import org.httpobjects.util.MimeTypeTool
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

object MarkdownToRevealJS extends App {
  import org.commonmark.parser.Parser
  
  val inputPath = new FilesystemPath(args(0))
  val outputPath = new FilesystemPath(args(1))
  
  val template = IOUtils.toString(getClass.getResourceAsStream("template.html"))
  
  println("Reading " + inputPath)
  val markdown = FileUtils.readFileToString(inputPath, Charset.forName("ASCII"))
  
  val parser = Parser.builder().build();
  
  val document = parser.parse(markdown);
    
  def streamNodes(document:Node):Stream[Node] = {
     
    def next(n:Node):Stream[Node] = {
      n match {
        case null => Stream.Empty
        case node => Stream.cons(node, next(n.getNext))
      }
    }
    next(document.getFirstChild)
  }
  
  val nodes = streamNodes(document).toList
  
  println(nodes.size + " nodes")
  
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
  
  def breakApartIfTooLarge(slide:Slide):List[Slide] = {
    
    val size = slide.body.foldLeft(0){(accum, slide) => accum + nestedNodeCount(slide)}
    
    
    
    
    val max = 10
    
    if(size>max){
      if(slide.body.size>1){
        println("TOO LARGE " + size + textContent(slide.h) + " " + slide.body.size)
        
        val children = slide.body
        val midPoint = children.size/2
        val buckets = List(children.slice(0, midPoint), children.slice(midPoint, children.size))
        buckets.flatMap{content=>
          println(" subsize " + content.size + " midpoint " + midPoint)
          val subSlide = Slide(slide.h, content)
          breakApartIfTooLarge(subSlide)
        }
      }else{
        slide.body.headOption match {
          case Some(ul:BulletList) => {
            val children = streamNodes(ul).toList
            val midPoint = children.size/2
            val buckets = List(children.slice(0, midPoint), children.slice(midPoint, children.size))
            buckets.map{bullets => 
              val list = new BulletList()
              bullets.foreach(list.appendChild(_))
              Slide(slide.h, List(list))
            }
          }
          case _ => List(slide)
        }
      }
      
    }else {
      List(slide)
    }
  }
  
  
  
  val expandedSlides = slides.flatMap(breakApartIfTooLarge)
  
  /*
   * TODO: Break things apart when they are too large
   */
  
  def print(n:Node, level:Int) {
    val indent = (for(n <- 0 to level) yield " ").mkString
    println(indent + n)
    
    streamNodes(n).foreach(print(_, level+1))
  }
  
  def nestedNodeCount(n:Node):Int = {
    streamNodes(n).foldLeft(0){(accum, child)=>
      accum + 1 + nestedNodeCount(child)
    }
  }
  
  def textContent(n:Node):String = {
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
        
        val text = textContent(h)
        
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
  
  val slidesHtml = expandedSlides.map({slide=>
    println("HEADER:" + toHtml(slide.h))
    val bodyHtml = slide.body.map(toSpecialHtml).mkString("\n")
    
    val id = streamNodes(slide.h).map(toHtml).mkString.filter(_.isLetter)
    
    slideTemplate
      .replaceAllLiterally("ID_GOES_HERE", id)
      .replaceAllLiterally("TITLE_GOES_HERE", toHtml(slide.h))
      .replaceAllLiterally("BODY_GOES_HERE", bodyHtml)
      
  }).mkString
  
  val outlineSlideHtml = outline.map({name=>
    println("OUTLINE: " + name)
     s"""<li>$name</li>"""
  }).mkString("<section><h1>Outline</h1><ul>", "\n", "</ul></section>")
  
  val html = template
        .replaceAllLiterally("TITLE_GOES_HERE", "howdy")
        .replaceAllLiterally("SLIDES_GO_HERE", outlineSlideHtml + slidesHtml)
        
  FileUtils.writeStringToFile(outputPath, html)
  println("Wrote to " + outputPath)
  
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
