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
      );
    }
    def parseClassDecl: ClassDecl= {
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
      return new ClassDecl(id, parent, vars, methods);
    }

    def parseMainDecl: MainDecl = {
      eat(OBJECT);
      val obj = parseIdentifier;
      eat(EXTENDS);
      val parent = parseIdentifier;
      val (vars, exprs, retExprs ) = parseMethodBody;
      //case class MainDecl(obj: Identifier, parent: Identifier, vars: List[VarDecl], exprs: List[ExprTree]) extends Tree
      return new MainDecl(obj, parent, vars, exprs:::List(retExprs));
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
      currentToken.kind match {
      	  case DEF 	=> eat(DEF);overrides=false;
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
        args :+= new Formal(arg_type, arg_name)

	      while(currentToken.kind != RPAREN){
		eat(COMMA);
		val arg_name= parseIdentifier;
		eat(COLON);
		val arg_type = parseType
		args :+= new Formal(arg_type, arg_name)
	      }
      }
      eat(RPAREN);
      eat(COLON);
      val retType=parseType;
      eat(EQSIGN);
      val (vars, exprs, retExpr ) = parseMethodBody;
      return new MethodDecl(overrides, retType, id, args, vars, exprs, retExpr); 
    }

   

    def parseVarDecl: VarDecl = {
      eat(VAR);
      val name = parseIdentifier;
      eat(COLON);
      val tpe = parseType;
      eat(EQSIGN);
      val expr = parseExpression;
      return new VarDecl(tpe,name,expr); 
    }
    def parseType: TypeTree = {
      currentToken.kind match {
        case INT =>  { readToken; new IntType(); } 
        case BOOLEAN =>{ readToken; new BooleanType(); }
        case STRING => { readToken; new StringType();}
        case UNIT => { readToken; new UnitType(); }
        case IDKIND => parseIdentifier
        case v => expected(INT, BOOLEAN, STRING,UNIT)
      }
    }
        
    def parseMaybeAccess(obj: ExprTree):ExprTree = {
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
         parseMaybeAccess (  new MethodCall(obj, meth,args));
        }
        else{
          eat(RPAREN);
          parseMaybeAccess ( new MethodCall(obj, meth, List()));
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
      if(currentToken.kind == EQSIGN){
        eat(EQSIGN);
        new Assign(id, parseExpression);
      }
      else if(MaybeAccessFirstSet.contains(currentToken.kind)){
        parseMaybeAccess(id);
      }
      else{
        expected(EQSIGN, MaybeAccessFirstSet:_*);
      }
    }


    def parseThing: ExprTree = {
      var thing: ExprTree = currentToken.kind match { 
        case TRUE   =>  parseMaybeAccess ( { eat(TRUE); new True();})
        case FALSE  => parseMaybeAccess (  {eat(FALSE); new False();})
        case IDKIND => IdentOrAssign( currentToken match{
                          case idtoken:ID => {eat(IDKIND); new Identifier(idtoken.value);}
                          case idtoken    => fatal("somthing imposible happend, IDKIND but not ID", idtoken);
                        })
        case THIS   =>  parseMaybeAccess ( {eat(THIS); new This();})
        case NULL   =>  parseMaybeAccess ( {eat(NULL); new Null();})
        case LBRACE =>  parseMaybeAccess ( {
                          eat(LBRACE); 
                          if(currentToken.kind == RBRACE){
                            eat(RBRACE);
                            new Block(List());
                          }
                          else{
                            var exprs: List[ExprTree] = List(parseExpression);
                            while(currentToken.kind != RBRACE){
                              eat(SEMICOLON);
                              exprs :+= parseExpression;
                            }
                            eat(RBRACE);
                            new Block(exprs);
                          }
                        })
        case IF     =>  parseMaybeAccess ( {
                          eat(IF); eat(LPAREN);
                          val expr = parseExpression;
                          eat(RPAREN);
                          val thn = parseExpression;
                          if(currentToken.kind == ELSE){
                            eat(ELSE);
                            new If(expr, thn, Some(parseExpression));
                          }
                          else{
                            new If(expr, thn, None);
                          }
                        })
        case WHILE  =>  parseMaybeAccess ( {
                          eat(WHILE);eat(LPAREN)
                          val cond = parseExpression;
                          eat(RPAREN);
                          val body = parseExpression;
                          new While(cond, body);
                        })
        case PRINTLN => parseMaybeAccess ( {
                          eat(PRINTLN);eat(LPAREN);
                          val expr=parseExpression;
                          eat(RPAREN);
                          new Println(expr);
                        })
        case NEW    =>  parseMaybeAccess ( {
                          eat(NEW);
                          val tpe = parseIdentifier;
                          eat(LPAREN);eat(RPAREN);
                          new New(tpe);
                        })
        case INTLITKIND =>  parseMaybeAccess ( currentToken match {
                              case intlit:INTLIT => {
                                  eat(INTLITKIND);
                                  new IntLit(intlit.value);
                                }
                              case intlit => fatal("somthing imposible happend, INTLITKIND but not INTLIT", intlit);
                            })
        case STRLITKIND =>  parseMaybeAccess ( currentToken match{
                              case strlit:STRLIT => {
                                  eat(STRLITKIND);
                                  new StringLit(strlit.value);
                                }
                              case strlit =>  fatal("somthing imposible happend, STRILITKIND but not STRLIT", strlit);
                            })
        case LPAREN => parseMaybeAccess (  {
                          eat(LPAREN);
                          val expr = parseExpression;
                          eat(RPAREN);
                          expr;
                        })
        case v      => expected(TRUE, FALSE, IDKIND, THIS, NULL, LBRACE, IF, WHILE, PRINTLN, NEW, INTLITKIND, STRLITKIND, LPAREN)
      }
      thing;
    }

    def parseFactor: ExprTree = {
      if(currentToken.kind==BANG){
        eat(BANG);
        val temp=parseThing;
        new Not(temp);
      }
      else if(List(TRUE, FALSE, IDKIND, THIS, NULL, LBRACE, IF, WHILE, PRINTLN, NEW, INTLITKIND, STRLITKIND, LPAREN).contains(currentToken.kind)){
        parseThing;
      }
      else{
        expected(BANG, TRUE, FALSE, IDKIND, THIS, NULL, LBRACE, IF, WHILE, PRINTLN, NEW, INTLITKIND, STRLITKIND, LPAREN)
      }
    }

    def parseFactorList(rhs: ExprTree): ExprTree = {
      if( currentToken.kind == TIMES){
        readToken;
        val lhs = parseFactor;
        parseFactorList(new Times(rhs, lhs));

      }
      else if(currentToken.kind == DIV ){
        readToken;
        val lhs = parseFactor;
        parseFactorList(new Div(rhs, lhs));
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
       if( currentToken.kind == PLUS){
        readToken;
        val lhs = parseTerm;
        parseTermList(new Plus(rhs, lhs));

      }
      else if(currentToken.kind == MINUS ){
        readToken;
        val lhs = parseTerm;
        parseTermList(new Minus(rhs, lhs));
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
       if( currentToken.kind == EQUALS){
        readToken;
        val lhs = parseCompTerm;
        parseCompList(new Equals(rhs, lhs));

      }
      else if(currentToken.kind == LESSTHAN ){
        readToken;
        val lhs = parseCompTerm;
        parseCompList(new LessThan(rhs, lhs));
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
        readToken;
        val lhs = parseAndTerm;
        parseAndList(new And(rhs, lhs));

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
        readToken;
        val lhs = parseOrTerm;
        parseOrList(new Or(rhs, lhs));

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
      var classes:  List[ClassDecl] = List();
      while(currentToken.kind==CLASS) classes :+= parseClassDecl;
      val res = new Program(parseMainDecl, classes);
      eat(EOF);
      return res;
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
