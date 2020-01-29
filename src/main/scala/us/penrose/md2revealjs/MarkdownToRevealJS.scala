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

  def params(): Map[String, Option[String]] ={
    val Pattern = "(.*)=(.*)".r

    args.toSeq.map{
      case Pattern(name, value) => (name -> Some(value))
      case arg => (arg -> None)
    }.toMap
  }
  val inputPath = new FilesystemPath(args(0))
  val outputPath = new FilesystemPath(args(1))
  val title = params.get("title").flatten.getOrElse("A Presentation")
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

    val markdown = FileUtils.readFileToString(inputPath, Charset.forName("ASCII"))
    
    val parser = Parser.builder().build();
    
    val document = parser.parse(markdown);

    val nodes = streamNodes(document).toList

    case class Accum(currentSection:Section, root:Section){
      def withSiblingSection(s:Section) = {
        println("adding sibling " + s.name + " after " + currentSection.name)
        val updatedTree = root.withSiblingAdded(currentSection, s)
        copy(
          currentSection = s,
          root = updatedTree)
      }

      def withNestedSection(s:Section) = {
        println("adding nested " + s.name + " under " + currentSection.name)
        withUpdatedCurrentSection(currentSection.copy(subsections = currentSection.subsections :+ s))

        val updatedCurrentSection = currentSection.copy(subsections = currentSection.subsections :+ s)
        copy(
          currentSection = s,
          root = root.withUpdate(currentSection, updatedCurrentSection))
      }

      def withUpdatedCurrentSection(updatedSection:Section) = {
        copy(
          currentSection = updatedSection,
          root = root.withUpdate(currentSection, updatedSection))
      }

      def withNewAtLevel(level:Int, s:Section) = {

        copy(
          currentSection = s,
          root = root.withNewAtLevel(level, s))
      }
    }

    trait DocumentSection
    case class Section(id:String, name:String, h:Heading, body:List[Node] = List(), level:Int, subsections:Seq[Section] = Seq()){
      def getSectionByName(name:String) : Option[Section] = {
        if(this.name == name) Some(this)
        else subsections.map{s=> s.getSectionByName(name)}.flatten.headOption
      }
      def withSiblingAdded(target:Section, newSibling:Section): Section ={
        if(subsections.contains(target)){
          copy(subsections = subsections :+ newSibling)
        }else{
          copy(subsections = subsections.map(_.withSiblingAdded(target, newSibling)))
        }
      }

      def withNewAtLevel(level:Int, s:Section) : Section = {
        if(this.level + 1 == level){
          copy(subsections = subsections :+ s)
        }else{
          // add it to the last
          val leading = subsections.take(subsections.size-1)
          val updatedLast = subsections.lastOption.map(_.withNewAtLevel(level, s))

          copy(subsections = leading ++ updatedLast)
        }
      }

      def withUpdate(target:Section, update:Section): Section ={
        if(target == this){
          update
        }else{
          copy(subsections = subsections.map(_.withUpdate(target, update)))
        }
      }
    }
    def mkId(h:Heading) = {
      streamNodes(h).map(toText).mkString.filter{char=> char.isLetter || char.isDigit || Set('-', '.', '_').contains(char)}
    }

    val emptyRoot = Section(id = "root", h = null, name = title , level = 0)
    val root = nodes.foldLeft(Accum(currentSection = emptyRoot, root = emptyRoot)){(accum, n)=>

      val currentSection = accum.currentSection

      n match {
        case h:Heading => {
          val newSection = Section(id = mkId(h),
            h = h,
            body = List(),
            name = streamNodes(h).map(toText).mkString, level = h.getLevel)

          accum.withNewAtLevel(h.getLevel, newSection)
        }
        case n:Node => {

          val updatedSection = currentSection.copy(body = currentSection.body :+ n)

          accum.withUpdatedCurrentSection(updatedSection)
        }
      }
    }.root


    def printS(s:Section, whitespace:String = ""): Unit ={
      println(whitespace + "Section: " + s.name + " (level " + s.level + ")  -  " + s.subsections.size + " subsections")

      s.subsections.foreach(printS(_, whitespace + " "))
    }

    printS(root, "")

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

    def renderSection(section:Section, outlineNameOverride:Option[String] = None):String = {

      val outline = if(section.subsections.size <= 1) "" else {
        val outline:Seq[String] = section.subsections.map(_.name)
        println("Outline for " + section.name + ": " + outline.mkString(", "))
        outline.map({name=>
          //    println("OUTLINE: " + name)
          s"""<li>$name</li>"""
        }).mkString(s"""<section id="${section.id + "-outline"}"><h2>${outlineNameOverride.getOrElse(section.name)}</h2><ul>""", "\n", "</ul></section>")
      }

      val slideHtml = if(section.body.isEmpty) "" else {
        val bodyHtml = section.body.map(toSpecialHtml).mkString("\n")

        slideTemplate
          .replaceAllLiterally("ID_GOES_HERE", section.id)
          .replaceAllLiterally("TITLE_GOES_HERE", toHtml(section.h))
          .replaceAllLiterally("BODY_GOES_HERE", bodyHtml)
      }

      val subsectionsHtml = section.subsections.map(renderSection(_, None)).mkString("\n\n")

      outline + slideHtml + subsectionsHtml
    }

    val effectiveRoot = if(root.body.isEmpty && root.subsections.size == 1) root.subsections.head else root

    val bodyHtml = renderSection(effectiveRoot, Some("Outline"))

    
    val html = template
          .replaceAllLiterally("TITLE_GOES_HERE", effectiveRoot.name)
          .replaceAllLiterally("SLIDES_GO_HERE",bodyHtml)
          
    FileUtils.writeStringToFile(outputPath, html)
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
