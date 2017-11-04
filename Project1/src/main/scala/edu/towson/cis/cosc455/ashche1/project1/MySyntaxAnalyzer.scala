package edu.towson.cis.cosc455.ashche1.project1

import scala.collection.mutable.{ArrayBuffer, Stack}

class MySyntaxAnalyzer extends SyntaxAnalyzer {
  var errorFound = false
  var context = new Stack[() => Unit]()

  var nodesStack = new Stack[Stack[Ast]]()

  var ast: Ast = _

  override def gittex(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      // add to parse tree / stack
      Compiler.Scanner.getNextToken()

      //println(s"Error: current token is $Compiler.currentToken")
      nodesStack.push( new Stack[Ast] )

      if (!errorFound) variableDefine()
      if (!errorFound) title()
      if (!errorFound) body()

      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        ast = new DocumentNode(nodesStack.pop().toList reverse) // final result of parsing - AST
      }
      else {
        errorFound = true
        println(s"Error: current token is ${Compiler.currentToken} expected ${CONSTANTS.DOCE}")
      }
    }
    else {
      errorFound = true
      println(s"Error: current token is ${Compiler.currentToken} expected ${CONSTANTS.DOCB}")
    }
  }

  override def paragraph(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.PARAB) {
      Compiler.Scanner.getNextToken()

      nodesStack.push(new Stack[Ast]())

      if (!errorFound) variableDefine()
      if (!errorFound) innerText()
      if (Compiler.currentToken equalsIgnoreCase CONSTANTS.PARAE) {

        val para = nodesStack.pop()
        if(! para.isEmpty) nodesStack.top.push( new ParagraphNode(para.toList reverse))

        Compiler.Scanner.getNextToken()
        innerText()

      } else {
        errorFound = true
        print("PARAE expected!")
      }

    }
  }

  override def innerItem(): Unit = {
    context.push(innerItem)
    if (!errorFound) variableUse()
    if (!errorFound) bold()
    if (!errorFound) link()
    if (!errorFound) {
      //TODO: check if the text is valid
      val freetext = Compiler.currentToken
      if( !Compiler.Scanner.lookup(freetext) ) { //if it's not a token
        nodesStack.top.push( new TextNode(freetext))

        Compiler.Scanner.getNextToken()
        innerText()
      }
    }
    context.pop()
  }

  override def innerText(): Unit = {
    context.push( innerText)
    if (!errorFound) variableUse()
    if (!errorFound) heading()
    if (!errorFound) bold()
    if (!errorFound) listItem()
    if (!errorFound) image()
    if (!errorFound) link()
    if (!errorFound) {
      //TODO: check if the text is valid
      val freetext = Compiler.currentToken
      if( !Compiler.Scanner.lookup(freetext) ) { //if it's not a token
        nodesStack.top.push( new TextNode(freetext))

        Compiler.Scanner.getNextToken()
        innerText()
      }
    }
    context.pop()
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
          val freetext2 = Compiler.currentToken
          Compiler.Scanner.getNextToken()
          val adde = Compiler.currentToken
          if (adde equalsIgnoreCase CONSTANTS.ADDRESSE) {
            nodesStack.top.push( new LinkNode(freetext,freetext2))

            Compiler.Scanner.getNextToken()
            context.top()
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

    }
  }

  override def italics(): Unit = {} // Requred by trait, so just add stub

  override def body(): Unit = {
    if (!errorFound) innerText()
    if (!errorFound) paragraph()
    if (!errorFound) newline()
  }

  override def bold(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.BOLD) {
      Compiler.Scanner.getNextToken()
      //TODO: check if text is valid or if its there at all. for now lets just assume that text must be there
      val freeText = Compiler.currentToken
      Compiler.Scanner.getText()
      val bld = Compiler.currentToken
      if (Compiler.currentToken equalsIgnoreCase CONSTANTS.BOLD) {
        nodesStack.top.push( new BoldNode(bld) )

        Compiler.Scanner.getNextToken()
        context.top()
      } else {
        errorFound = true
        print("Bold expected!")
      }
    }
  }

  override def newline(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase("\n")) {
      nodesStack.top.push( new NewLineNode() )

      Compiler.Scanner.getNextToken()
    }
  }

  override def title(): Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      Compiler.Scanner.getText()
      val titleText = Compiler.currentToken
      Compiler.Scanner.getNextToken()
      val brackete = Compiler.currentToken
      if (brackete.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        Compiler.Scanner.getNextToken()

        nodesStack.top.push( new TitleNode(titleText) )

      } else {
        errorFound = true
        print(s"Error: current token is $Compiler.currentToken expected $CONSTANTS.BRACKETE")
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

          nodesStack.top.push( new VarDefNode(varName, varValue))

          Compiler.Scanner.getNextToken()
          variableDefine()
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
          val freetext2 = Compiler.currentToken
          Compiler.Scanner.getNextToken()
          val adresse = Compiler.currentToken
          if (adresse equalsIgnoreCase CONSTANTS.ADDRESSE) {

            nodesStack.top.push( new ImageNode(freetext, freetext2) )

            Compiler.Scanner.getNextToken()
            innerText()

          } else {
            errorFound = true
            println("ADDRESSE expected!")
          }


        } else {
          errorFound = true
          println("ADDRESSB expected!")
        }

      }
    }
  }

  override def variableUse(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.USEB) {
      Compiler.Scanner.getNextToken()
      //Text is required
      val varname = Compiler.currentToken
      //TODO: check if we have any text
      Compiler.Scanner.getNextToken()
      val brackete = Compiler.currentToken
      if (brackete equalsIgnoreCase CONSTANTS.BRACKETE) {

        nodesStack.top.push( new VarUseNode(varname) )

        Compiler.Scanner.getNextToken()
        context.top()
      } else {
        errorFound = true
        println("Error: Expected BRACKETE, have ??? instead")
      }
    }
  }


  override def heading(): Unit = {
    if (Compiler.currentToken equalsIgnoreCase CONSTANTS.HEADING) {
      Compiler.Scanner.getText()
      val freetext = Compiler.currentToken
      Compiler.Scanner.getNextToken()
      //Text is required
        //TODO: check if we have any text
        nodesStack.top.push( new HeadingNode(freetext) )
    }
  }

  override def listItem(): Unit = {
    if(Compiler.currentToken equalsIgnoreCase CONSTANTS.LISTITEM){
      Compiler.Scanner.getNextToken()
      if (!errorFound) {
        nodesStack.push( new Stack[Ast]() )
        innerItem()

        val list =  nodesStack.pop()
        nodesStack.top.push( new ListItemNode( list.toList reverse) )

        listItem()
      }
    }
  }
}