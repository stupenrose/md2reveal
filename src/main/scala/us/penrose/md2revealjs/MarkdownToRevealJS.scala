package us.penrose.md2revealjs

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
  
  
  
  
  val expandedSlides = slides//.flatMap(SlideReducer.breakApartIfTooLarge(_, 5))
  
  
  
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
