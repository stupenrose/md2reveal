package us.penrose.md2revealjs

import us.penrose.md2revealjs.CommonmarkUtil._
import us.penrose.md2revealjs.Util._
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

  def params(): Map[String, Option[String]] ={
    val Pattern = "(.*)=(.*)".r

    args.toSeq.map{
      case Pattern(name, value) => (name -> Some(value))
      case arg => (arg -> None)
    }.toMap
  }
  val inputPath = new FilesystemPath(args(0))
  val outputPath = new FilesystemPath(args(1))
  val title = args(2)
  val headingLevel = params.get("slide-heading-level").flatten.map(_.toInt).getOrElse(1)
  
  var lastModified = 0L;
  do{
    if(lastModified < inputPath.lastModified()){
      println("rendering")
      render(inputPath, outputPath, title)
      println("done")
      lastModified = inputPath.lastModified();
    }else{
      Thread.sleep(500)
    }
    
  }while(true);
  
  def render(inputPath:FilesystemPath, outputPath:FilesystemPath, title:String){
    import org.commonmark.parser.Parser
    
    
    val template = IOUtils.toString(getClass.getResourceAsStream("template.html"))
    
  //  println("Reading " + inputPath)
    val markdown = FileUtils.readFileToString(inputPath, Charset.forName("ASCII"))
    
    val parser = Parser.builder().build();
    
    val document = parser.parse(markdown);
    
    def reduceHeadings(n:Node){
      n match {
        case h:Heading => h.setLevel(h.getLevel + headingLevel)
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
          slides :+ Slide(
                        id = streamNodes(h).map(toText).mkString.filter{char=> char.isLetter || char.isDigit || Set('-', '.', '_').contains(char)},
                        h = h, 
                        body = List())
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
    }.filterNot{slide=> textSubContent(slide.h).trim().toLowerCase() == "hidden"}
    
    
    val expandedSlides = {
      val slidesById = groupSequential(slides.toList)({slide=> slide.id})
      
      slidesById.flatMap{case (id, group)=>
        if(group.size>1){
          group.zipWithIndex.map{case (slide, idx) =>
            val num = idx + 1
            slide.copy(id = if(num>1) slide.id + num else slide.id)
          }
        }else {
          group
        }
      }
      
    }//.flatMap(SlideReducer.breakApartIfTooLarge(_, 5))
    
    def nestedNodeCount(n:Node):Int = {
      streamNodes(n).foldLeft(0){(accum, child)=>
        accum + 1 + nestedNodeCount(child)
      }
    }
    
    val outline = slides.flatMap{slide=>
      slide.h match {
        case h:Heading => {
          
          val text = textSubContent(h)
          
          h.getLevel match {
            case 2 => List(text)
            case _ => List()
          }
          
        }
        case _:Node => List()
      }
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
      
      slideTemplate
        .replaceAllLiterally("ID_GOES_HERE", slide.id)
        .replaceAllLiterally("TITLE_GOES_HERE", toHtml(slide.h))
        .replaceAllLiterally("BODY_GOES_HERE", bodyHtml)
        
    }).mkString
    
    val outlineSlideHtml = outline.map({name=>
  //    println("OUTLINE: " + name)
       s"""<li>$name</li>"""
    }).mkString("""<section id="Outline"><h1>Outline</h1><ul>""", "\n", "</ul></section>")
    
    val html = template
          .replaceAllLiterally("TITLE_GOES_HERE", title)
          .replaceAllLiterally("SLIDES_GO_HERE", outlineSlideHtml + slidesHtml)
          
    FileUtils.writeStringToFile(outputPath, html)
  //  println("Wrote to " + outputPath)
  }
  
  
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
