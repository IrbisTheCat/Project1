import edu.towson.cis.cosc455.ashche1.project1.{Compiler, MyLexicalAnalyzer, MySyntaxAnalyzer}
val Scanner = new MyLexicalAnalyzer
val Parser = new MySyntaxAnalyzer
val SemanticAnalyzer = new MySyntaxAnalyzer
print("wtf")
Scanner.SourceLine = "\\BEGIN\r\n\\END"

Parser.errorFound


Scanner.getNextToken()

Scanner.lexeme
//Parser.gittex()

Parser.errorFound
