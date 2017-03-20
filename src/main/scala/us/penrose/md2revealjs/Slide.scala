package us.penrose.md2revealjs

import org.commonmark.node._

case class Slide(h:Heading, body:List[Node])