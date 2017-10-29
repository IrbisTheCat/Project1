package edu.towson.cis.cosc455.ashche1.project1

trait LexicalAnalyzer {
  def addChar(): Unit

  def getChar(): Char

  def getNextToken(): Unit

  def lookup(candidateToken: String): Boolean = {
    println("this is lookup implementation!")
    true
  }
}
