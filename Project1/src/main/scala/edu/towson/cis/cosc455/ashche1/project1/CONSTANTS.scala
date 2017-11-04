package edu.towson.cis.cosc455.ashche1.project1

object CONSTANTS {
  val letters : List[Char] = ('A' to 'Z' toList) ::: ('a' to 'z' toList)
  val numbersEtc : List[Char] = List('1','2','3','4','5','6','7','8','9','0',
    ',','.','\'',':','?','_','/', ''')// ''
  val whiteSpace : List[Char] = List(' ', '\t', '\b','\f') //'\n', ,'\r'

  val validText : List[Char] = whiteSpace ::: letters ::: numbersEtc

  //val otherChars: List[String]= List(DOCB,DOCE,TITLEB, BRACKETE, HEADING, PARAB, PARAE, BOLD, LISTITEM, NEWLINE,LINKB, ADDRESSB, ADDRESSE, IMAGEB, DEFB, EQSIGN, USEB)

  val DOCB : String = 	"\\BEGIN"
  val DOCE : String = 	"\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE : String = "]"
  val HEADING : String= "#"
  val PARAB :String ="\\PARAB"
  val PARAE :String = "\\PARAE"
  val BOLD :String= "*"
  val LISTITEM :String= "+"
  val NEWLINE :String= "\\\\"
  val LINKB :String= "["
  val ADDRESSB :String= "("
  val ADDRESSE :String= ")"
  val IMAGEB :String= "!["
  val DEFB :String="\\DEF["
  val EQSIGN :String= "="
  val USEB :String= "\\USE["


  val lexems: List[String] = List(DOCB,DOCE,TITLEB, BRACKETE, HEADING, PARAB, PARAE, BOLD, LISTITEM, NEWLINE,LINKB, ADDRESSB, ADDRESSE, IMAGEB, DEFB, EQSIGN, USEB)

}
