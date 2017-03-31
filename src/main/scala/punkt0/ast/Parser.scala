package punkt0
package ast

import Trees._
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  val parseFactorListFirstSet = List(COMMA, TIMES, DIV, RBRACE, SEMICOLON, RPAREN, ELSE, PLUS, MINUS, LESSTHAN, EQUALS, AND, OR);
  val parseFactorListFollowSet = List(COMMA,            RBRACE, SEMICOLON, RPAREN, ELSE, PLUS, MINUS, LESSTHAN, EQUALS, AND, OR);
  
  val parseTermListFirstSet = List(COMMA, PLUS, MINUS, RBRACE, SEMICOLON, RPAREN, ELSE, LESSTHAN, EQUALS, AND, OR);
  val parseTermListFollowSet = List(COMMA,             RBRACE, SEMICOLON, RPAREN, ELSE, LESSTHAN, EQUALS, AND, OR);

  val parseCompListFirstSet = List(COMMA, RBRACE, SEMICOLON, RPAREN, ELSE, LESSTHAN, EQUALS, AND, OR);
  val parseCompListFollowSet = List(COMMA, RBRACE, SEMICOLON, RPAREN, ELSE,                  AND, OR);

  val parseAndListFirstSet = List(COMMA, RBRACE, SEMICOLON, RPAREN, ELSE, AND, OR);
  val parseAndListFollowSet = List(COMMA, RBRACE, SEMICOLON, RPAREN, ELSE,     OR);
  
  val parseOrListFirstSet = List(COMMA, RBRACE, SEMICOLON, RPAREN, ELSE, OR);
  val parseOrListFollowSet = List(COMMA, RBRACE, SEMICOLON, RPAREN, ELSE);
  
  val MaybeAccessFirstSet = List(COMMA, DOT, RBRACE, SEMICOLON, RPAREN, ELSE, TIMES, DIV, PLUS, MINUS,LESSTHAN,EQUALS,AND,OR);
  val MaybeAccessFollowSet = List(COMMA,     RBRACE, SEMICOLON, RPAREN, ELSE, TIMES, DIV, PLUS, MINUS,LESSTHAN,EQUALS,AND,OR);
  
  var bad = false;
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)
  
    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          bad = true;
          error("Syntax error",currentToken);
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }
    

    def parseIdentifier: Identifier = {
      val tempTock = currentToken;
      eat(IDKIND);
      return new Identifier( 
        tempTock match {
      	  case v: ID => v.value
          case v => expected(IDKIND)
      	}
      ).setPos(tempTock);
    }
    def parseClassDecl: ClassDecl= {
      var pos:Positioned = currentToken;
      eat(CLASS);
      val id = parseIdentifier;
      var parent : Option[Identifier] = None;
      if(currentToken.kind==EXTENDS){
        eat(EXTENDS);
        parent = Some(parseIdentifier);
      }
      eat(LBRACE)

      var vars: List[VarDecl] = List();
      while(currentToken.kind==VAR){
        vars :+= parseVarDecl;
        eat(SEMICOLON);
      }
 
      var methods: List[MethodDecl] = List();
      while(currentToken.kind==DEF || currentToken.kind==OVERRIDE){
        methods :+= parseMethodDecl;
      }
      eat(RBRACE);
      return new ClassDecl(id, parent, vars, methods).setPos(pos);
    }

    def parseMainDecl: MainDecl = {
      var pos:Positioned = currentToken;
      eat(OBJECT);
      val obj = parseIdentifier;
      eat(EXTENDS);
      val parent = parseIdentifier;
      val (vars, exprs, retExprs ) = parseMethodBody;
      //case class MainDecl(obj: Identifier, parent: Identifier, vars: List[VarDecl], exprs: List[ExprTree]) extends Tree
      return new MainDecl(obj, parent, vars, exprs:::List(retExprs)).setPos(pos);
    }

    def parseMethodBody: (List[VarDecl], List[ExprTree], ExprTree) = {
      var vars: List[VarDecl] = List();
      var expr: List[ExprTree] = List();
      eat(LBRACE);
      while(currentToken.kind==VAR){
         vars :+= parseVarDecl;
         eat(SEMICOLON);
      }
      expr :+= parseExpression;
      while(currentToken.kind==SEMICOLON){
        eat(SEMICOLON);
        expr :+= parseExpression;
   
      }
      val retExpr=expr(expr.size-1);
      expr=expr.dropRight(1);
      eat(RBRACE);
      return (vars, expr, retExpr);
    }

    def parseMethodDecl: MethodDecl = {
      var overrides=false;
      var pos:Positioned = currentToken;
      currentToken.kind match {
      	  case DEF 	=> eat(DEF); overrides=false; 
          case OVERRIDE => eat(OVERRIDE); overrides=true; eat(DEF)
          case v 	=> expected(DEF, OVERRIDE)
      }
 
      val id = parseIdentifier;
      eat(LPAREN);
      var args: List[Formal] = List();
      if(currentToken.kind!=RPAREN){
        val arg_name= parseIdentifier;
        eat(COLON);
        val arg_type = parseType
        args :+= new Formal(arg_type, arg_name).setPos(arg_name)

	      while(currentToken.kind != RPAREN){
          eat(COMMA);
          val arg_name= parseIdentifier;
          eat(COLON);
          val arg_type = parseType
          args :+= new Formal(arg_type, arg_name).setPos(arg_name);
	      }
      }
      eat(RPAREN);
      eat(COLON);
      val retType=parseType;
      eat(EQSIGN);
      val (vars, exprs, retExpr ) = parseMethodBody;
      return new MethodDecl(overrides, retType, id, args, vars, exprs, retExpr).setPos(pos); 
    }

   

    def parseVarDecl: VarDecl = {
      var pos:Positioned = currentToken;
      eat(VAR);
      val name = parseIdentifier;
      eat(COLON);
      val tpe = parseType;
      eat(EQSIGN);
      val expr = parseExpression;
      return new VarDecl(tpe,name,expr).setPos(pos); 
    }
    def parseType: TypeTree = {
      var pos:Positioned = currentToken;
      currentToken.kind match {
        case INT =>  { readToken; new IntType().setPos(pos); } 
        case BOOLEAN =>{ readToken; new BooleanType().setPos(pos); }
        case STRING => { readToken; new StringType().setPos(pos);}
        case UNIT => { readToken; new UnitType().setPos(pos); }
        case IDKIND => parseIdentifier
        case v => expected(INT, BOOLEAN, STRING,UNIT)
      }
    }
        
    def parseMaybeAccess(obj: ExprTree):ExprTree = {
      var pos:Positioned = currentToken;
      if(currentToken.kind==DOT){
        eat(DOT)
        val meth=parseIdentifier;

        eat(LPAREN);
        if(currentToken.kind != RPAREN){
          var args : List[ExprTree] = List(parseExpression);
          while(currentToken.kind!=RPAREN){
            eat(COMMA);
            args :+= parseExpression;
          }
          eat(RPAREN)
         parseMaybeAccess (  new MethodCall(obj, meth,args).setPos(pos));
        }
        else{
          eat(RPAREN);
          parseMaybeAccess ( new MethodCall(obj, meth, List()).setPos(pos));
        }
      }
      else if(MaybeAccessFollowSet.contains(currentToken.kind)){
        obj;
      }
      else{
        expected(MaybeAccessFirstSet.head, MaybeAccessFirstSet.tail:_*);
      }
    }
    
    def IdentOrAssign ( id: Identifier ) : ExprTree = {
      var pos:Positioned = currentToken;
      if(currentToken.kind == EQSIGN){
        eat(EQSIGN);
        new Assign(id, parseExpression).setPos(pos);
      }
      else if(MaybeAccessFirstSet.contains(currentToken.kind)){
        parseMaybeAccess(id);
      }
      else{
        expected(EQSIGN, MaybeAccessFirstSet:_*);
      }
    }


    def parseThing: ExprTree = {
      var pos:Positioned = currentToken;
      var thing: ExprTree = currentToken.kind match { 
        case TRUE   =>  parseMaybeAccess ( { eat(TRUE); new True().setPos(pos);})
        case FALSE  => parseMaybeAccess (  {eat(FALSE); new False().setPos(pos);})
        case IDKIND => IdentOrAssign( currentToken match{
                          case idtoken:ID => {eat(IDKIND); new Identifier(idtoken.value).setPos(pos);}
                          case idtoken    => fatal("somthing imposible happend, IDKIND but not ID", idtoken);
                        })
        case THIS   =>  parseMaybeAccess ( {eat(THIS); new This().setPos(pos);})
        case NULL   =>  parseMaybeAccess ( {eat(NULL); new Null().setPos(pos);})
        case LBRACE =>  parseMaybeAccess ( {
                          eat(LBRACE); 
                          if(currentToken.kind == RBRACE){
                            eat(RBRACE);
                            new Block(List()).setPos(pos);
                          }
                          else{
                            var exprs: List[ExprTree] = List(parseExpression);
                            while(currentToken.kind != RBRACE){
                              eat(SEMICOLON);
                              exprs :+= parseExpression;
                            }
                            eat(RBRACE);
                            new Block(exprs).setPos(pos);
                          }
                        })
        case IF     =>  parseMaybeAccess ( {
                          eat(IF); eat(LPAREN);
                          val expr = parseExpression;
                          eat(RPAREN);
                          val thn = parseExpression;
                          if(currentToken.kind == ELSE){
                            eat(ELSE);
                            new If(expr, thn, Some(parseExpression)).setPos(pos);
                          }
                          else{
                            new If(expr, thn, None).setPos(pos);
                          }
                        })
        case WHILE  =>  parseMaybeAccess ( {
                          eat(WHILE);eat(LPAREN)
                          val cond = parseExpression;
                          eat(RPAREN);
                          val body = parseExpression;
                          new While(cond, body).setPos(pos);
                        })
        case PRINTLN => parseMaybeAccess ( {
                          eat(PRINTLN);eat(LPAREN);
                          val expr=parseExpression;
                          eat(RPAREN);
                          new Println(expr).setPos(pos);
                        })
        case NEW    =>  parseMaybeAccess ( {
                          eat(NEW);
                          val tpe = parseIdentifier;
                          eat(LPAREN);eat(RPAREN);
                          new New(tpe).setPos(pos);
                        })
        case INTLITKIND =>  parseMaybeAccess ( currentToken match {
                              case intlit:INTLIT => {
                                  eat(INTLITKIND);
                                  new IntLit(intlit.value).setPos(pos);
                                }
                              case intlit => fatal("somthing imposible happend, INTLITKIND but not INTLIT", intlit);
                            })
        case STRLITKIND =>  parseMaybeAccess ( currentToken match{
                              case strlit:STRLIT => {
                                  eat(STRLITKIND);
                                  new StringLit(strlit.value).setPos(pos);
                                }
                              case strlit =>  fatal("somthing imposible happend, STRILITKIND but not STRLIT", strlit);
                            })
        case LPAREN => parseMaybeAccess (  {
                          eat(LPAREN);
                          val expr = parseExpression;
                          eat(RPAREN);
                          expr;
                        })
        case v      => expected(TRUE,
          FALSE,
          IDKIND,
          THIS,
          NULL,
          LBRACE,
          IF,
          WHILE,
          PRINTLN,
          NEW,
          INTLITKIND,
          STRLITKIND,
          LPAREN)
      }
      thing;
    }

    def parseFactor: ExprTree = {
      var pos:Positioned = currentToken;
      if(currentToken.kind==BANG){
        eat(BANG);
        val temp=parseThing;
        new Not(temp).setPos(pos);
      }
      else if(List(TRUE, 
                    FALSE, 
                    IDKIND, 
                    THIS, 
                    NULL, 
                    LBRACE, 
                    IF, WHILE, 
                    PRINTLN, NEW, 
                    INTLITKIND, 
                    STRLITKIND, 
                    LPAREN).contains(currentToken.kind)){
        parseThing;
      }
      else{
        expected(BANG,
          TRUE,
          FALSE,
          IDKIND,
          THIS,
          NULL, 
          LBRACE,
          IF,
          WHILE,
          PRINTLN,
          NEW,
          INTLITKIND,
          STRLITKIND,
          LPAREN)
      }
    }

    def parseFactorList(rhs: ExprTree): ExprTree = {
      var pos:Positioned = currentToken;
      if( currentToken.kind == TIMES){
        readToken;
        val lhs = parseFactor;
        parseFactorList(new Times(rhs, lhs).setPos(pos));

      }
      else if(currentToken.kind == DIV ){
        readToken;
        val lhs = parseFactor;
        parseFactorList(new Div(rhs, lhs).setPos(pos));
      }
      else if (parseFactorListFollowSet.contains(currentToken.kind)){
        rhs;
      }
      else{
        expected(parseFactorListFirstSet.head, parseFactorListFirstSet.tail:_*);
      }
    }
    
    def parseTerm: ExprTree = {
        val rhs = parseFactor;
        parseFactorList(rhs);
    }

    def parseTermList(rhs: ExprTree): ExprTree = {
      var pos:Positioned = currentToken;
       if( currentToken.kind == PLUS){
        readToken;
        val lhs = parseTerm;
        parseTermList(new Plus(rhs, lhs).setPos(pos));

      }
      else if(currentToken.kind == MINUS ){
        readToken;
        val lhs = parseTerm;
        parseTermList(new Minus(rhs, lhs).setPos(pos));
      }
      else if (parseTermListFollowSet.contains(currentToken.kind)){
        rhs;
      }
      else{
        expected(parseTermListFirstSet.head, parseTermListFirstSet.tail:_*);
      }
    }
    
    def parseCompTerm: ExprTree = {
        val rhs = parseTerm;
        parseTermList(rhs);
    }

    def parseCompList(rhs: ExprTree): ExprTree = {
      var pos:Positioned = currentToken;
      if( currentToken.kind == EQUALS){
        readToken;
        val lhs = parseCompTerm;
        parseCompList(new Equals(rhs, lhs).setPos(pos));

      }
      else if(currentToken.kind == LESSTHAN ){
        readToken;
        val lhs = parseCompTerm;
        parseCompList(new LessThan(rhs, lhs).setPos(pos));
      }
      else if (parseCompListFollowSet.contains(currentToken.kind)){
        rhs;
      }
      else{
        expected(parseCompListFirstSet.head, parseCompListFirstSet.tail:_*);
      }
    }
    
    def parseAndTerm: ExprTree = {
        val rhs = parseCompTerm;
        parseCompList(rhs);
    }

    def parseAndList(rhs: ExprTree): ExprTree = {
      if( currentToken.kind == AND){
        var pos:Positioned = currentToken;
        readToken;
        val lhs = parseAndTerm;
        parseAndList(new And(rhs, lhs).setPos(pos));

      }
      else if (parseAndListFollowSet.contains(currentToken.kind)){
        rhs;
      }
      else{
        expected(parseAndListFirstSet.head, parseAndListFirstSet.tail:_*);
      }
    }

    def parseOrTerm: ExprTree = {
        val rhs = parseAndTerm;
        parseAndList(rhs);
    }

    def parseOrList(rhs: ExprTree): ExprTree = {
      if( currentToken.kind == OR){
        var pos:Positioned = currentToken;
        readToken;
        val lhs = parseOrTerm;
        parseOrList(new Or(rhs, lhs).setPos(pos));
      }
      else if (parseOrListFollowSet.contains(currentToken.kind)){
        rhs;
      }
      else{
        expected(parseOrListFirstSet.head, parseOrListFirstSet.tail:_*);
      }
    }


    def parseExpression: ExprTree = {
        val rhs = parseOrTerm;
        parseOrList(rhs);
    }

    def parseGoal: Program = {
      var pos:Positioned = currentToken;
      var classes:  List[ClassDecl] = List();
      while(currentToken.kind==CLASS) classes :+= parseClassDecl;
      val res = new Program(parseMainDecl, classes).setPos(pos);
      eat(EOF);
      return res;
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
