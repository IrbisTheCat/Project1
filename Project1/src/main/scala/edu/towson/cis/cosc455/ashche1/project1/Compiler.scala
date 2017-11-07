package edu.towson.cis.cosc455.ashche1.project1

import java.io.PrintWriter
import java.io.File

import java.awt.Desktop
import java.io.{File, IOException}


import scala.collection.mutable.ListBuffer


/** Compiler class is responsible for taking in command line arguments
  * checking them, instantiating the Lexical, Syntax and Semantic Analyzers
  * and calling on them.
  */

object Compiler {
  var currentToken: String = ""
  var fileContents: String = ""

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
    if (!Parser.errorFound) {
      SemanticAnalyzer.generate(Parser.ast)
      val result = SemanticAnalyzer.result()
      print(result)
      val writer = new PrintWriter(new File("output.html"))
      writer.write(result)
      writer.close()
      openHTMLFileInBrowser("output.html")
    }

  }

  /** Reads text from a source file into a single String object
    *
    * @param file the file that it reads from */

  def readFile(file: String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  /** Checks for correctness of arguments
    *
    * @param args arguments from the user */

  def checkFile(args: Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: wrong number of args fool!")
      System.exit(1)
    }
    else if (!args(0).endsWith(".gtx")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }

  /** Opens the output of the program in the browser
    *
    * @param htmlFileStr HTML file to be displayed */

  def openHTMLFileInBrowser(htmlFileStr: String) = {
    val file: File = new File(htmlFileStr.trim)
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
