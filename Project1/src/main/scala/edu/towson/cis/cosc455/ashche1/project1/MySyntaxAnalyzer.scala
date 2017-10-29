package edu.towson.cis.cosc455.ashche1.project1

import edu.towson.cis.cosc455.ashche1.project1

class MySyntaxAnalyzer extends SyntaxAnalyzer {
  var errorFound: Boolean = false

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      // add to parse tree / stack
      Compiler.Scanner.getNextToken()

      if (!errorFound) variableDefine()
      if (!errorFound) title()
      if (!errorFound) body()

      if (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        println("Error: current token is ?? expected ???")
      }
    }
    else {
      println("Error")
      System.exit(1)
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.PARAB) {
      Compiler.Scanner.getNextToken()
      if (!errorFound) variableDefine()
      if (!errorFound) innerText()
      if (Compiler.currentToken equalsIgnoreCase CONSTANTS.PARAE) {
        Compiler.Scanner.getNextToken()
      } else {
        errorFound = true
        print("PARAE expected!")
      }
    } else {
      errorFound = true
      print("PARAB expected!")
    }
  }

  override def innerItem(): Unit = {
    if (!errorFound) {
      variableUse();
      innerItem()
    }
    if (!errorFound) {
      bold();
      innerItem()
    }
    if (!errorFound) {
      link();
      innerItem()
    }
    if (!errorFound) {
      //TODO: check if the text is valid
      val freetext = Compiler.currentToken
      Compiler.Scanner.getNextToken()
      innerItem()
    }
  }

  override def innerText(): Unit = {
    if (!errorFound) {
      variableUse();
      innerText()
    }
    if (!errorFound) {
      heading();
      innerText()
    }
    if (!errorFound) {
      bold();
      innerText()
    }
    if (!errorFound) {
      listItem();
      innerText()
    }
    if (!errorFound) {
      image();
      innerText()
    }
    if (!errorFound) {
      link();
      innerText()
    }
    if (!errorFound) {
      //TODO: check if the text is valid
      val freetext = Compiler.currentToken
      Compiler.Scanner.getNextToken()
      innerText()
    }
  }

  override def link(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.LINKB) {
      Compiler.Scanner.getNextToken()
      //TODO: check if the text is valid and is there to begin with
      val freetext = Compiler.currentToken
      Compiler.Scanner.getNextToken()
      val brack = Compiler.currentToken
      if (brack equalsIgnoreCase CONSTANTS.BRACKETE) {
        Compiler.Scanner.getNextToken()
        val addb = Compiler.currentToken
        if (addb equalsIgnoreCase CONSTANTS.ADDRESSB) {
          Compiler.Scanner.getNextToken()
          //TODO: check if the text is valid and is there to begin with
          val freetext = Compiler.currentToken
          Compiler.Scanner.getNextToken()


          val adde = Compiler.currentToken
          if (adde equalsIgnoreCase CONSTANTS.ADDRESSE) {
            Compiler.Scanner.getNextToken()
          } else {
            errorFound = true
            println("ADDRESSE expected!")
          }


        } else {
          errorFound = true
          println("ADDRESSB expected!")
        }


      } else {
        errorFound = true
        println("BRACKETE expected!")
      }


    } else {
      errorFound = true
      println("LINKB expected!")
    }
  }

  //override def italics(): Unit = ???

  override def body(): Unit = {
    if (!errorFound) {
      innerText();
      body()
    }
    if (!errorFound) {
      paragraph();
      body()
    }
    if (!errorFound) {
      newline();
      body()
    }
  }

  override def bold(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.BOLD) {
      Compiler.Scanner.getNextToken()
      //TODO: check if text is valid or if its there at all. for now lets just assume that text must be there
      val freeText = Compiler.currentToken
      Compiler.Scanner.getNextToken()
      val bld = Compiler.currentToken
      if (Compiler.currentToken equalsIgnoreCase CONSTANTS.BOLD) {
        Compiler.Scanner.getNextToken()

      } else {
        errorFound = true
        print("Bold expected!")
      }

    } else {
      errorFound = true
      print("Bold expected!")
    }
  }

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase("\n")) {
      Compiler.Scanner.getNextToken()
    } else {
      errorFound = true
      print("Newline expected!")
    }
  }

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      Compiler.Scanner.getNextToken()
      val titleText = Compiler.currentToken
      Compiler.Scanner.getNextToken()
      val brackete = Compiler.currentToken
      if (brackete.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        Compiler.Scanner.getNextToken()
      } else {
        errorFound = true
        print("BRACKETE expected!")
      }
    }
  }

  override def variableDefine(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      Compiler.Scanner.getNextToken()
      val varName = Compiler.currentToken
      Compiler.Scanner.getNextToken()
      val eqSign = Compiler.currentToken
      if (eqSign.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
        Compiler.Scanner.getNextToken()
        val varValue = Compiler.currentToken
        Compiler.Scanner.getNextToken()
        val brackete = Compiler.currentToken
        if (brackete.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
          Compiler.Scanner.getNextToken()
        } else {
          errorFound = true
          print("BRACKETE expected!")
        }
      } else {
        errorFound = true
        print("EQSIGN expected!")
      }
    }
  }

  override def image(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.IMAGEB) {
      Compiler.Scanner.getNextToken()
      val freetext = Compiler.currentToken
      //TODO: check if text is there, it is required
      Compiler.Scanner.getNextToken()
      val brac = Compiler.currentToken
      if (brac equalsIgnoreCase CONSTANTS.BRACKETE) {
        Compiler.Scanner.getNextToken()
        val addb = Compiler.currentToken
        if (addb equalsIgnoreCase CONSTANTS.ADDRESSB) {
          Compiler.Scanner.getNextToken()
          //TODO: check if the text is valid and is there to begin with
          val freetext = Compiler.currentToken
          Compiler.Scanner.getNextToken()
          val adde = Compiler.currentToken
          if (adde equalsIgnoreCase CONSTANTS.ADDRESSE) {
            Compiler.Scanner.getNextToken()
          } else {
            errorFound = true
            println("ADDRESSE expected!")
          }


        } else {
          errorFound = true
          println("ADDRESSB expected!")
        }

      }
      else {
        errorFound = true
        println("Error: Expected IMAGE, have ??? instead")
      }
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.USEB) {
      Compiler.Scanner.getNextToken()
      //Text is required
      val freetext = Compiler.currentToken
      //TODO: check if we have any text
      Compiler.Scanner.getNextToken()
      val brackete = Compiler.currentToken
      if (brackete equalsIgnoreCase CONSTANTS.BRACKETE) {
        Compiler.Scanner.getNextToken()
      } else {
        errorFound = true
        println("Error: Expected BRACKETE, have ??? instead")
      }
    } else {
      errorFound = true
      println("Error: Expected USEB, have ??? instead")
    }
  }


  override def heading(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.HEADING) {
      Compiler.Scanner.getNextToken()

      //Text is required
      val freetext = Compiler.currentToken
      //TODO: check if we have any text

    } else {
      errorFound = true
      println("Error: Expected Heading, have ??? instead")
    }
  }

  override def listItem(): Unit = {
    if(Compiler.currentToken equalsIgnoreCase CONSTANTS.LISTITEM){
      Compiler.Scanner.getNextToken()
      if (!errorFound) {
       innerItem(); listItem()
      }
    } else{
      errorFound = true
      println("Error: Expected LISTITEM, have ??? instead")
    }
  }
}
