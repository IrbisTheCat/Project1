package edu.towson.cis.cosc455.ashche1.project1

//TODO Stop accpeting Russian letters :(

/** Lexical analyzer is responsible for reading file character by character and gathering them into tokens
  * I believe I took some of those methods from the example compiler we looked at in class */

class MyLexicalAnalyzer extends LexicalAnalyzer {
  var position: Int = 0
  var SourceLine: String = ""
  var nextChar: Char = ' '
  var lexeme = new StringBuilder;


  /** Adds character to the lexeme */
  override def addChar(): Unit = {
    lexeme.append(nextChar)
  }

  /** Gets next token */

  override def getNextToken(): Unit = {
    lexeme.clear()

    getNonBlank()

    var break = false
    while (
      !break
        && !(CONSTANTS.lexems contains lexeme.toString)
        && !(CONSTANTS.whiteSpace contains nextChar)
        && !(CONSTANTS.lexems contains lexeme.toString())
        && nextChar != 4
    ) {
      lexeme append nextChar
      getChar()
      if (nextChar == ']' || nextChar == ')' || nextChar == '\r' || nextChar == '\n') {
        break = true
      }
    }

    val newToken = lexeme.toString()

    Compiler.currentToken = newToken
  }

  /** Method that gets text that is not token, just normal text */

  def getText(): Unit = {
    lexeme.clear()
    var break = false
    while (
      (CONSTANTS.validText contains nextChar)
        && nextChar != 4
    ) {
      lexeme append nextChar
      getChar()
    }

    val newToken = lexeme.toString()

    Compiler.currentToken = newToken
  }

  /** Looks up if the token is valid
    *
    * @param candidateToken token to be checked */

  override def lookup(candidateToken: String): Boolean = {
    if (!CONSTANTS.lexems.contains(candidateToken)) {
      if(candidateToken.length>1) {
        if (candidateToken.charAt(0) == '\\' && candidateToken.charAt(1) != '\\') {
          println(s"Lexical Error! Token $candidateToken is invalid!")
          System.exit(0)
        }
      }

      return false
    }
    return true


  }

  /** This method gets the next character from the "program" string. */

  override def getChar(): Char = {
    if (position < SourceLine.length) nextChar = SourceLine.charAt({
      position += 1;
      position - 1
    })
    else nextChar = 4
    nextChar
  }


  /** A helper method to determine if the current character is a space.
    *
    * @param c the character to be checked */

  def isSpace(c: Char) = c == ' '


  /** A helper method to determine if the current character is newline or carriage return
    *
    * @param c the character to be checked */

  def isNl(c: Char) = c == '\r' || c == '\n'

  /** A helper method to get the next non-blank character. */

  def getNonBlank(): Unit = {
    while ( {
      isSpace(nextChar) || isNl(nextChar) || nextChar == '\t'
    }) getChar()
  }


}