package punkt0
package analyzer

import ast.Trees._
import Symbols._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._
    // Step 1: Collect symbols in declarations
    var globalScope = new Symbols.GlobalScope
    globalScope.classes += {"App" -> new ClassSymbol("App")}

    def collectClasses(cl: ClassDecl){
		       
      globalScope.classes get cl.id.value match{
        case None     =>{
          var classSym = new Symbols.ClassSymbol(cl.id.value).setPos(cl);
		      cl.setSymbol(classSym);
          cl.id.setSymbol(classSym);
          globalScope.classes += (cl.id.value -> classSym);
        }
        case Some(x)  =>  error( " class " + cl.id.value + " defined twice.\nFirst defined here:" , x, "then redefined here:",cl);

      }
		}

    def collectClassMembers(cl: ClassDecl){
		  cl.vars.map(collectFields(_,cl.getSymbol))
      cl.methods.map(collectMethods(_,cl.getSymbol))
    }

    def linkClassParrents(cl: ClassDecl){
      cl.parent match{
        case None     => {}
      case Some(x)  => globalScope.lookupClass(x.value,true, x).map((x) => {cl.getSymbol.parent = Some(x)});
      }
    }
    def linkMainParrents(cl: MainDecl){
      globalScope.lookupClass(cl.parent.value,true, cl.parent).map((x) => {cl.getSymbol.parent= Some(x)});
    }

    def doYouLoop(cl: ClassDecl){
      var chain: List[ClassSymbol] = Nil
      var current_class:Option[ClassSymbol]=Some(cl.getSymbol)
      while(current_class match { case Some(v : Symbol) => true ; case v => false}){
        if(chain.contains(current_class.get))
          error("llegal cyclic reference involving class"+cl.id.value, cl)
        else{
          chain :+= current_class.get;
          current_class=current_class.get.parent;
        }
      }
    }
  	def collectMethods(meth: MethodDecl, classScope: ClassSymbol){
      def constructSymbol: MethodSymbol = {
	        var methSym = new Symbols.MethodSymbol(meth.id.value, classScope).setPos(meth)
  		    meth.setSymbol(methSym)
          meth.id.setSymbol(methSym);
          classScope.methods += (meth.id.value -> methSym)
  		    meth.args.map(x => collectArgs(x, methSym))
  		    meth.vars.map(x => collectVars(x, methSym))
          return methSym;
      }
      classScope.lookupMethod(meth.id.value) match{
        case None => 	constructSymbol;
        case Some(x)    => {
          classScope.lookupVar(meth.id.value) match{
            case Some(x) => error( " method " + meth.id.value + " overrides a variable.\n Defined fined here:" , x, " and here:",meth);
            case None   => {
              if((meth.overrides) && // overrides 
              (classScope.methods get meth.id.value match { case None => true; case Some(v) => false;}) && // and undefinid in this class
              (x.argList.length == meth.args.length)) { // and the same number of arguments 
                  val methSym=constructSymbol;
                  methSym.overridden=Some(x);
                }
              else error( " method " + meth.id.value + " defined twice.\nFirst defined here:" , x, "then redefined here:",meth)
            }
          }
        }
      }
  	}
  	def collectMain(main: MainDecl){
  		var classSym = new Symbols.ClassSymbol(main.obj.value).setPos(main)
  		main.setSymbol(classSym)
      main.obj.setSymbol(classSym);
  		globalScope.mainClass =  classSym
      linkMainParrents(main);
  		main.vars.map(collectFields(_, classSym))
  	}

  	def collectFields(v: VarDecl, scope: ClassSymbol){
      scope.lookupVar(v.id.value) match{
        case None => scope.lookupMethod(v.id.value) match{
          case None=>{
            var sym = new Symbols.VariableSymbol(v.id.value).setPos(v)
            v.setSymbol(sym)
            v.id.setSymbol(sym);
            scope.members += (v.id.value -> sym)
          }
          case Some(x) => error( " class feild " + v.id.value + " is shadowing a method.\nMethod defined here:" , x, "feild defined here:",v)
        }
        case Some(x) => error( " class feild " + v.id.value + " defined twice, or shadowing parrant feild.\nFirst defined here:" , x, "then redefined here:",v)
      }
  	}

    def checkFieldInharatanceShadowing(v: VarDecl, scope: ClassSymbol){
      scope.parent match{
        case None => {}
        case Some(parent) =>{
          parent.lookupVar(v.id.value) match{
            case None => parent.lookupMethod(v.id.value) match{
              case None=>{}
              case Some(x) => error( " class feild " + v.id.value + " is shadowing a method.\nMethod defined here:" , x, "feild defined here:",v)
            }
            case Some(x) => error( " class feild " + v.id.value + " defined twice, or shadowing parrant feild.\nFirst defined here:" , x, "then redefined here:",v)
          }
        }
      }
    }

	  def checkMethodInharatenceOverloading(meth: MethodDecl, classScope: ClassSymbol){
      classScope.parent match{
        case None => {}
        case Some(parent) => {
          parent.lookupMethod(meth.id.value) match{
            case None => 	{}
            case Some(x)    => {
              parent.lookupVar(meth.id.value) match{
                case Some(x) => error( " method " + meth.id.value + " overrides a variable.\n Defined fined here:" , x, " and here:",meth);
                case None   => {
                  if((meth.overrides) && // overrides 
                  (x.argList.length == meth.args.length)) { }// and the same number of arguments 
                  else error( " method " + meth.id.value + " defined twice.\nFirst defined here:" , x, "then redefined here:",meth)
                }
              }
            }
          }
        }
      }
    }

  	def collectArgs(v: Formal, scope: MethodSymbol){
      scope.params get v.id.value match{
        case Some(x)  => error( " argument " + v.id.value + " defined twice.\nFirst defined here:" , x, "then redefined here:", v);
        case None     => {
          var sym:VariableSymbol = new Symbols.VariableSymbol(v.id.value).setPos(v)
  		    v.setSymbol(sym)
          v.id.setSymbol(sym);
	        scope.params += (v.id.value -> sym);
          scope.argList :+= sym;
        }
      }
  	}
  	def collectVars(v: VarDecl, scope: MethodSymbol){
      scope.params get v.id.value match{
        case None => scope.params get v.id.value match {
          case None=> {
            var sym = new Symbols.VariableSymbol(v.id.value).setPos(v)
  		      v.setSymbol(sym)
            v.id.setSymbol(sym);
  		      scope.members += (v.id.value -> sym)
          }
          case Some(x) => error( " variable " + v.id.value + " is shadowing a method argument defined here:" , x, "variable declared here:", v);
        }
        case Some(x) => error( " variable " + v.id.value + " defined twice.\nFirst defined here:" , x, "then redefined here:", v);
      }
  	}

  	prog.classes.map(collectClasses(_));
    prog.classes.map(linkClassParrents(_));
    prog.classes.map(doYouLoop(_));
    prog.classes.map(collectClassMembers(_));
    prog.classes.map(cl => {
      cl.vars.map(checkFieldInharatanceShadowing(_,cl.getSymbol));
      cl.methods.map(checkMethodInharatenceOverloading(_,cl.getSymbol));
    })
  	collectMain(prog.main);


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    def attachSymInExpr(ex:ExprTree, scope: Either[ClassSymbol, MethodSymbol]){
      ex match{
        case exp:And        => attachSymInExpr(exp.lhs, scope);attachSymInExpr(exp.rhs, scope);
        case exp:Or         => attachSymInExpr(exp.lhs, scope);attachSymInExpr(exp.rhs, scope);
        case exp:Plus       => attachSymInExpr(exp.lhs, scope);attachSymInExpr(exp.rhs, scope);
        case exp:Minus      => attachSymInExpr(exp.lhs, scope);attachSymInExpr(exp.rhs, scope);
        case exp:Times      => attachSymInExpr(exp.lhs, scope);attachSymInExpr(exp.rhs, scope);
        case exp:Div        => attachSymInExpr(exp.lhs, scope);attachSymInExpr(exp.rhs, scope);
        case exp:LessThan   => attachSymInExpr(exp.lhs, scope);attachSymInExpr(exp.rhs, scope);
        case exp:Equals     => attachSymInExpr(exp.lhs, scope);attachSymInExpr(exp.rhs, scope);
        case exp:MethodCall => attachSymInExpr(exp.obj, scope); exp.args.map( attachSymInExpr(_, scope));

        case exp:Identifier =>  scope match{
                                  case Left(cscope) => cscope.lookupVar(exp.value,true, exp).map(exp.setSymbol(_));
                                  case Right(mscope)=> mscope.lookupVar(exp.value,true, exp).map(exp.setSymbol(_));
                                }
        case exp:This     =>  scope match{
                                  case Left(cscope)=> exp.setSymbol(cscope);
                                  case Right(mscope)=> exp.setSymbol(mscope.classSymbol);
                                }
        case exp:New      => globalScope.lookupClass(exp.tpe.value,true, exp.tpe).map(exp.tpe.setSymbol(_));
        case exp:Not      => attachSymInExpr(exp.expr, scope);

        case exp:Block    =>  exp.exprs.map(attachSymInExpr(_, scope));
        case exp:If       =>  attachSymInExpr(exp.expr, scope); attachSymInExpr(exp.thn, scope);exp.els.map(attachSymInExpr(_, scope));
        case exp:While    =>  attachSymInExpr(exp.cond, scope); attachSymInExpr(exp.body, scope); 
        case exp:Println  =>  attachSymInExpr(exp.expr, scope);
        case exp:Assign   =>  attachSymInExpr(exp.expr, scope);
                              scope match{
                                case Left(cscope) => cscope.lookupVar(exp.id.value,true, exp.id).map(exp.id.setSymbol(_));
                                case Right(mscope)=> mscope.lookupVar(exp.id.value,true, exp.id).map(exp.id.setSymbol(_));
                              }
        case exp          => {};
      }
    }
  
    prog.classes.map( (cl)=> {
      cl.parent match {
        case None => {};
        case Some(parent) => {
          globalScope.lookupClass(parent.value, true, parent).map(parent.setSymbol(_));
        }
      }
      cl.vars.map(x => {
          attachSymInExpr(x.expr, Left(cl.getSymbol));
          x.tpe match{
            case tpe:Identifier => globalScope.lookupClass(tpe.value,true,tpe).map(tpe.setSymbol(_));
            case tpe            => {};
          }
        }
      )
      cl.methods.map( mth => {
        mth.retType match{
          case r:Identifier =>  globalScope.lookupClass(r.value,true, r).map(r.setSymbol(_));
          case r            => {}
        }
        mth.args.map(x =>  x.tpe match{
            case tpe:Identifier => globalScope.lookupClass(tpe.value,true,tpe).map(tpe.setSymbol(_));
            case tpe            => {};
          }
        )
        mth.vars.map(x=> attachSymInExpr(x.expr, Right(mth.getSymbol)))
        mth.exprs.map(attachSymInExpr(_, Right(mth.getSymbol)));
        attachSymInExpr(mth.retExpr, Right(mth.getSymbol));
      });
    })
    prog.main.vars.map(v =>{ 
      attachSymInExpr(v.expr, Left(prog.main.getSymbol));
      v.tpe match{
        case tpe:Identifier => globalScope.lookupClass(tpe.value,true,tpe).map(tpe.setSymbol(_));
        case tpe            => {};
      }
    });
    prog.main.exprs.map(attachSymInExpr(_, Left(prog.main.getSymbol)))
    prog.setSymbol(globalScope)
    prog
  }

}
