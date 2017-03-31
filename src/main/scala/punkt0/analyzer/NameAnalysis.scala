package punkt0
package analyzer

import ast.Trees._
import Symbols._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._
    // Step 1: Collect symbols in declarations
    var globalScope = new Symbols.GlobalScope;
    def collectClasses(cl: ClassDecl){
		var classSym = new Symbols.ClassSymbol(cl.id.value)
		cl.setSymbol(classSym)
		globalScope.classes += (cl.id.value -> classSym)
		cl.methods.map(x => collectMethods(x,classSym))
		cl.vars.map(x => collectMembers(x,classSym))
  	}
  	def collectMethods(meth: MethodDecl, classScope: ClassSymbol){
  		var methSym = new Symbols.MethodSymbol(meth.id.value, classScope)
  		meth.setSymbol(methSym)
  		classScope.methods += (meth.id.value -> methSym)
  		meth.args.map(x => collectArgs(x, methSym))
  		meth.vars.map(x => collectVars(x, methSym))
  	}
  	def collectMain(main: MainDecl){
  		var classSym = new Symbols.ClassSymbol(main.obj.value)
  		main.setSymbol(classSym)
  		globalScope.classes += (main.obj.value -> classSym)
  		main.vars.map(x => collectMembers(x, classSym))
  	}
  	def collectMembers(v: VarDecl, scope: ClassSymbol){
  		var sym = new Symbols.VariableSymbol(v.id.value)
  		v.setSymbol(sym)
  		scope.members += (v.id.value -> sym)
  	}
  	def collectArgs(v: Formal, scope: MethodSymbol){
  		var sym = new Symbols.VariableSymbol(v.id.value)
  		v.setSymbol(sym)
  		scope.members += (v.id.value -> sym)
  	}
  	def collectVars(v: VarDecl, scope: MethodSymbol){
  		var sym = new Symbols.VariableSymbol(v.id.value)
  		v.setSymbol(sym)
  		scope.members += (v.id.value -> sym)
  	}
  	prog.classes.map(x => collectClasses(x))
  	collectMain(prog.main)


    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }

}
