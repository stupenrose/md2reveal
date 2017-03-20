package us.penrose.md2revealjs

import org.commonmark.node._

case class Slide(id:String, h:Heading, body:List[Node])