package edu.towson.cis.cosc455.ashche1.project1

import scala.collection.mutable
import scala.collection.mutable.{HashMap, Stack}

class MySemanticAnalyzer {
  val builder = new StringBuilder

  val context = new Stack[HashMap[String, String]]

  var inAList = false

  def generate(ast: Ast): Unit = {
    ast match {
      case DocumentNode(innerNodes) => {
        context.push(new mutable.HashMap[String, String]())
        builder.append("<html>")
        for(node <- innerNodes) { generate(node); }
        builder.append("</html>")
        context.pop()
      }

      case VarDefNode(name, value) => {
        context.top.put(name,value)
      }

      case TitleNode(text) => {
        builder.append("<head>")
          .append("<title>")
          .append(text)
          .append("</title>")
          .append("</head>")
      }

      case TextNode(text) => {
        builder.append(text)
          .append(" ")
      }

      case VarUseNode(name) => {
        builder.append(lookup(name))
          .append(" ")
      }

      case ParagraphNode(innerNodes) => {
        context.push(new mutable.HashMap[String, String]())
        builder.append("<p>")
        for(node <- innerNodes) { generate(node); }
        builder.append("</p>")
        context.pop()
      }


      case HeadingNode(test) => {
        builder.append("<h1>")
               .append(test)
               .append("</h1>")
      }

      case BoldNode(text)=>{
        builder.append("<b>")
          .append(text)
          .append("</b>")
      }

      case NewLineNode()=>{
        builder.append("\n")
      }
      case ImageNode(text, url)=>{
        builder.append("<img src=\"")
        builder.append(url)
        builder.append("\"")
        builder.append("alt=\"")
        builder.append(text)
        builder.append("\">")
      }
      case LinkNode(text, url)=> {
        builder.append("<a href=\"")
        builder.append(url)
        builder.append("\">")
        builder.append(text)
        builder.append("</a>")
        builder.append(" ")

      }


      case ListItemNode(innerNodes) if !inAList =>{
        context.push(new mutable.HashMap[String, String]())
        builder.append("<li>")
        inAList = true
        for(node <- innerNodes) {
          generate(node);
          }
        inAList = false
        builder.append("</li>")
        context.pop()
    }

      case ListItemNode(innerNodes) if inAList =>{
        context.push(new mutable.HashMap[String, String]())
        builder.append("</li><li>")
        for(node <- innerNodes) {
          generate(node);
        }
        //if(inAList) builder.append("</li>")
        context.pop()
      }
    }

  }
  def lookup(name:String): String = {
    val variable = context.find( d => d.contains(name))
    if(variable == null || variable.toString=="None") {
      print(s"ERROR: No variable $name is defined")
      sys.exit(1)
    }
    variable.get(name)
  }


  def result(): String = { builder toString }
}
