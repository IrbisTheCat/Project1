package edu.towson.cis.cosc455.ashche1.project1

abstract class Ast { } //Just a common inheritance root

//See https://docs.scala-lang.org/tour/pattern-matching.html on case classes
case class VarDefNode(val Name: String, val Value: String) extends Ast { }
case class VarUseNode(val Name: String)  extends Ast { }
case class TextNode(val text: String)  extends Ast { }
case class TitleNode(val text: String)  extends Ast { }
case class BoldNode(val text: String)  extends Ast { }
case class LinkNode(val text: String, val url: String)  extends Ast { }
case class ImageNode(val text: String, val url:String)  extends Ast { }
case class NewLineNode()  extends Ast { }
case class ListItemNode( val innerNodes: List[Ast]) extends Ast { }


case class HeadingNode(val test:String) extends Ast { }
//Gittext
case class DocumentNode( val innerNodes: List[Ast]) extends Ast { }
//Paragraph
case class ParagraphNode( val innerNodes: List[Ast]) extends Ast { }