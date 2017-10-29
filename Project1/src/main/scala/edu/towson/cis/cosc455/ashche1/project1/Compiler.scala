package edu.towson.cis.cosc455.ashche1.project1

import scala.collection.mutable.ListBuffer

object Compiler {
  var currentToken : String = ""
  var fileContents : String = ""
  //var tokens : ListBuffer[String] = ListBuffer()

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySyntaxAnalyzer

  def main(args: Array[String]): Unit = {
    //checks argumens
    checkFile(args)
    readFile(args(0))

    Scanner.SourceLine = fileContents;

   Scanner.getNextToken()


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
    else if (! args(0).endsWith(".mkd")) {
      println("USAGE ERROR: wrong extension fool!")
      System.exit(1)
    }
  }
}
