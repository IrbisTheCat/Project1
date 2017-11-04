package edu.towson.cis.cosc455.ashche1.project1


class MyLexicalAnalyzer extends LexicalAnalyzer {
  var position: Int = 0
  var SourceLine: String = ""
  var nextChar: Char = ' '
  var lexeme = new StringBuilder;

  /*
	This method adds the current character the the token after checking to make
	sure that the length of the token isn't too long, a lexical error in this
	case.
	*/
  override def addChar(): Unit = {
      lexeme.append(nextChar)
  }
  // override def lookup(): Boolean = ???

  override def getNextToken(): Unit = {
    lexeme.clear()

    // Ignore spaces and add the first character to the token
    getNonBlank()


    var break = false
      while (
        !break
        && !(CONSTANTS.lexems contains nextChar)
        && !(CONSTANTS.whiteSpace contains nextChar)
        && !(CONSTANTS.lexems contains lexeme.toString())
        && nextChar != 4
      ) {
        lexeme append nextChar
        getChar()
        if (nextChar == ']' || nextChar == ')'|| nextChar == '\r'|| nextChar == '\n') {
          break = true
        }
      }

    // Convert the gathered character array token into a String
    val newToken = lexeme.toString()

    //if (lookup(newToken)) Compiler.currentToken_$eq(newToken)
    Compiler.currentToken = newToken
  }

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

    // Convert the gathered character array token into a String
    val newToken = lexeme.toString()

    //if (lookup(newToken)) Compiler.currentToken_$eq(newToken)
    Compiler.currentToken = newToken
  }

  override def lookup(candidateToken: String): Boolean = {
    CONSTANTS.lexems.contains(candidateToken)
  }

  // This method gets the next character from the "program" string.
  override def getChar(): Char = {
    if (position < SourceLine.length) nextChar = SourceLine.charAt({
      position += 1; position - 1
    })
    else nextChar = 4  //EOF
    //print(nextChar)
    nextChar
  }

  // A helper method to determine if the current character is a space.
  def isSpace(c: Char) = c == ' '

  def isNl(c: Char) = c == '\r' || c == '\n'

  // A helper method to get the next non-blank character.
  def getNonBlank(): Unit = {
    while ( {
      isSpace(nextChar) || isNl(nextChar)
    }) getChar()
  }

  def peek(): Char = {
    if (position < SourceLine.length) SourceLine.charAt(position) else 4  //EOF
  }
}

