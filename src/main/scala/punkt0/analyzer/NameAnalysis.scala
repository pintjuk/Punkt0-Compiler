package punkt0
package analyzer

import ast.Trees._
import Symbols._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._
    // Step 1: Collect symbols in declarations
    var globalScope = new Symbols.GlobalScope

    def collectClasses(cl: ClassDecl){
		  var classSym = new Symbols.ClassSymbol(cl.id.value).setPos(cl)
		  cl.setSymbol(classSym)
      
      globalScope.classes get cl.id.value match{
        case None     =>  globalScope.classes += (cl.id.value -> classSym)
        case Some(x)  =>  error( " class " + cl.id.value + " defined twice.\nFirst defined here:" , x, "then redefined here:",cl);

      }
		  //cl.methods.map(x => collectMethods(x,classSym))
		  //cl.vars.map(x => collectMembers(x,classSym))
  	}

    def linkClassParrents(cl: ClassDecl){
      cl.parent match{
        case None     => {}
        case Some(x)  => globalScope.lookupClass(x.value) match {
          case None               => error("class "+ x.value + "is undefiend", x);
          case Some(parSymbol) => cl.getSymbol.parent= Some(parSymbol);
        }
      }
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
  		var methSym = new Symbols.MethodSymbol(meth.id.value, classScope).setPos(meth)
  		meth.setSymbol(methSym)
      classScope.methods get meth.id.value match{
        case None => 	{
          classScope.methods += (meth.id.value -> methSym)
  		    meth.args.map(x => collectArgs(x, methSym))
  		    meth.vars.map(x => collectVars(x, methSym))
        }
        case Some(x)    => error( " method " + meth.id.value + " defined twice.\nFirst defined here:" , x, "then redefined here:",meth)
      }
  	}
  	def collectMain(main: MainDecl){
  		var classSym = new Symbols.ClassSymbol(main.obj.value).setPos(main)
  		main.setSymbol(classSym)
  		globalScope.mainClass =  classSym
  		main.vars.map(x => collectMembers(x, classSym))
  	}
  	def collectMembers(v: VarDecl, scope: ClassSymbol){
  		var sym = new Symbols.VariableSymbol(v.id.value).setPos(v)
  		v.setSymbol(sym)
  		scope.members += (v.id.value -> sym)
  	}
  	def collectArgs(v: Formal, scope: MethodSymbol){
  		var sym = new Symbols.VariableSymbol(v.id.value).setPos(v)
  		v.setSymbol(sym)
  		scope.members += (v.id.value -> sym)
  	}
  	def collectVars(v: VarDecl, scope: MethodSymbol){
  		var sym = new Symbols.VariableSymbol(v.id.value).setPos(v)
  		v.setSymbol(sym)
  		scope.members += (v.id.value -> sym)
  	}
  	prog.classes.map(x => collectClasses(x));
    prog.classes.map(x => linkClassParrents(x));
    prog.classes.map(x => doYouLoop(x));
  	collectMain(prog.main)


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }

}
