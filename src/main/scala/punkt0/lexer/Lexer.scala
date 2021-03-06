package punkt0
package lexer

/*
TODO: does not lexer last token just writes EOF, fails if "==" 
*/
import java.io.File
import scala.util.control._


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._
  val keywords = Seq(
                    "object" -> new Token(OBJECT),
                    "class" -> new Token(CLASS),
                    "def" -> new Token(DEF),
                    "override" -> new Token(OVERRIDE),
                    "var" -> new Token(VAR),
                    "Unit" -> new Token(UNIT),
                    "String" -> new Token(STRING),
                    "extends" -> new Token(EXTENDS),
                    "Int" -> new Token(INT),
                    "Boolean" -> new Token(BOOLEAN),
                    "while" -> new Token(WHILE),
                    "if" -> new Token(IF),
                    "else" -> new Token(ELSE),
                    "true" -> new Token(TRUE),
                    "false" -> new Token(FALSE),
                    "this" -> new Token(THIS),
                    "null" -> new Token(NULL),
                    "new" -> new Token(NEW),
                    "println" -> new Token(PRINTLN)).toMap;

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    new Iterator[Token] {
      var empty=false;
      var parsedEOF=false;
      def hasNext = {
        !parsedEOF;
        /*if(source.nonEmpty){
          true;
        }
        else{
          if(!empty) { empty = true; true;}
          else false;
        }*/
      }
      def next: Token = {
        val pos = source.pos;
        if(!source.nonEmpty){ 
          parsedEOF=true;
          return registerPosition(new Token(EOF));
        }
        def registerPosition(t : Token): Token = {
          t.setPos(ctx.file.get,pos);
          t;
        }
        source.ch match{
          case '(' => source.next; registerPosition(new Token(LPAREN));
          case ')' => source.next; registerPosition(new Token(RPAREN));
          case ':' => source.next; registerPosition(new Token(COLON));
          case ';' => source.next; registerPosition(new Token(SEMICOLON));
          case '.' => source.next; registerPosition(new Token(DOT));
          case ',' => source.next; registerPosition(new Token(COMMA));
          case '=' => source.next; if(source.ch=='='){ source.next;
                                                       registerPosition(new Token(EQUALS))}
                                   else              { registerPosition(new Token(EQSIGN))}
          case '!' => source.next; registerPosition(new Token(BANG));
          case '{' => source.next; registerPosition(new Token(LBRACE));
          case '}' => source.next; registerPosition(new Token(RBRACE));
          case '&' => source.next; if(source.ch=='&'){source.next;
                                                      registerPosition(new Token(AND))}
                                   else              {val res=registerPosition(new Token(BAD)); error("Syntax error!", res); res}
          case '|' => source.next; if(source.ch=='|'){source.next;
                                                      registerPosition(new Token(OR))}
                                   else              {val res=registerPosition(new Token(BAD)); error("Syntax error!", res); res}
          case '<' => source.next; registerPosition(new Token(LESSTHAN));
          case '+' => source.next; registerPosition(new Token(PLUS));
          case '-' => source.next; registerPosition(new Token(MINUS));
          case '*' => source.next; registerPosition(new Token(TIMES));
          case '/' => {
            source.next; 
            if(source.ch=='/') {
              source.takeWhile(x => !(x=='\n' || x.isControl=='\r') ).mkString;
              return this.next;
            }
            else if(source.ch=='*') {
              do {
                source.takeWhile( x => x!='*').mkString
                if(!source.nonEmpty) {val res= registerPosition(new Token(BAD)); error("Syntax error!", res); return res}      
                source.next
              }while(source.ch!='/');
              source.next;
              return this.next;
            }
            else {
              return registerPosition(new Token(DIV));
            }
          }
          case '\"' =>{
            val result = registerPosition(new STRLIT( source.takeWhile( (x) => x!='\"' ).mkString));
            if(source.ch=='\"'){
               source.next;
               return result;
            }
            else{
              if(!source.nonEmpty) {
                val res = registerPosition(new Token(BAD));
                error("Syntax error!", res); 
                return  res;
              }
              else {
                source.next; 
                val res = registerPosition(new Token(BAD)); 
                error("Syntax error!", res);
                return res;
              }
            }
          }
          case v => {
            if( source.ch.isControl || source.ch.isSpaceChar || source.ch.isWhitespace ){
              source.takeWhile(x => x.isControl || x.isSpaceChar || x.isWhitespace).mkString;
              return this.next;
            }
            else if(v.isDigit){
               var result = 0;
               var numDigits =0;
               val loop = new Breaks;
               if(v=='0'){
                 source.next;
                 return registerPosition(new INTLIT(0));
               }
               loop.breakable{
                 while(source.ch.isDigit ){
                   numDigits=numDigits+1;
                   result = result*10+source.ch.getNumericValue;
                   if(!source.nonEmpty) loop.break;
                   source.next;
                 }
               }
               // TODO: Error check that int does not overflow
               //source.next;
               if(v=='0' && numDigits > 1) { val res = registerPosition(new Token(BAD)) ; error("Syntax error!", res); res} 
               else {registerPosition(new INTLIT(result))};
            }else if(v.isLetter){
               val result = v.toString + source.takeWhile( (x) => x.isLetter || x.isDigit || x=='_').mkString
               registerPosition(keywords.getOrElse(result, new ID(result)))
            }else {source.next; val res= registerPosition(new Token(BAD)); error("Syntax error!", res); res}
          }
        }
      }
    }
  }
}
