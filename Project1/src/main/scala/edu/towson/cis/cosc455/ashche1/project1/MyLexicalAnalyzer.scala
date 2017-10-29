package edu.towson.cis.cosc455.ashche1.project1


class MyLexicalAnalyzer extends LexicalAnalyzer {
  var position: Int = 0
  var SourceLine: String = ""
  var nextChar: Char = _
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

    // Continue gathering characters for the token
    while (nextChar != 4) {

      if(nextChar == '\\' ) { //consume aplhanum
        lexeme.append(nextChar)
        getChar()
        while(CONSTANTS.letters.contains(nextChar)) {
          lexeme.append(nextChar)
          getChar()
        }
        if(lookup(lexeme.toString())) {
          Compiler.currentToken = lexeme.toString()
          Compiler.Parser.gittex()
        } else {
          print( s"Unknown token {lexeme.toString()}")
          sys.exit(1)
        }
      }

      if(nextChar == '#') {
        while ( (nextChar != 4) && (nextChar != '\r') && (nextChar != '\n')) {
          getChar()
          lexeme.append(nextChar)
        }
        Compiler.currentToken = lexeme.toString()
        Compiler.Parser.heading()
      }

      if(nextChar == '+') {
        while ( (nextChar != 4) && (nextChar != '\r') && (nextChar != '\n')) {
          getChar()
          lexeme.append(nextChar)
        }
        Compiler.currentToken = lexeme.toString()
        Compiler.Parser.listItem()
      }

      if(nextChar == '[') {
        while ( (nextChar != ']') ) {
          getChar()
          lexeme.append(nextChar)
        }
        Compiler.currentToken = lexeme.toString()
        Compiler.Parser.listItem()
      }

      addChar()
      getChar()
    }

    // Convert the gathered character array token into a String
    val newToken = lexeme.toString()

    if (lookup(newToken)) Compiler.currentToken_$eq(newToken)
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
    nextChar
  }

  // A helper method to determine if the current character is a space.
  def isSpace(c: Char) = c == ' '

  // A helper method to get the next non-blank character.
  def getNonBlank(): Unit = {
    while ( {
      isSpace(nextChar)
    }) getChar()
  }

}

