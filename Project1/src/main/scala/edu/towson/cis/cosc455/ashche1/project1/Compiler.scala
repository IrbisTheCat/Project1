package edu.towson.cis.cosc455.ashche1.project1
import java.io.PrintWriter
import java.io.File

import java.awt.Desktop
import java.io.{File, IOException}


import scala.collection.mutable.ListBuffer

object Compiler {
  var currentToken : String = ""
  var fileContents : String = ""
  //var tokens : ListBuffer[String] = ListBuffer()

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {
    //checks argumens
    checkFile(args)
    readFile(args(0))

    Scanner.SourceLine = fileContents;

   Scanner.getNextToken()

    Parser.gittex()
    if(!Parser.errorFound){
      SemanticAnalyzer.generate(Parser.ast)
      val result = SemanticAnalyzer.result()
      print(result)
      val writer = new PrintWriter(new File("output.html"))
      writer.write(result)
      writer.close()
      openHTMLFileInBrowser("output.html")
    }

  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
}

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}
