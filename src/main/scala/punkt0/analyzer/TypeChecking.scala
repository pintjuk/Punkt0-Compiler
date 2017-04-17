package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {
    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = ??? // TODO: Compute type for each kind of expression


      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        Reporter.error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    val globalScope = prog.getSymbol;

    def makeType(tpe : TypeTree): Type = {
      tpe match {
        case BooleanType()  => TBoolean
        case StringType()   => TString
        case IntType()      => TInt
        case UnitType()     => TUnit
        case v:Identifier =>  globalScope.lookupClass(v.value) match {
          case None => { Reporter.error("class"+ v.value +"undefined", tpe); TError }
          case Some(v)    => new TClass(v)
        }
      }
    }
    def attachTypeToSymFormal( v : Formal ){
      v.getSymbol.setType(makeType(v.tpe))
    }

    def attachTypeToSymVar( v : VarDecl ){
      v.getSymbol.setType(makeType(v.tpe))
    }

    
    /*attach type to symbols*/
    prog.classes.map(cl => {
      cl.vars.map(attachTypeToSymVar(_)); 
      cl.methods.map(mth => {
        mth.vars.map(attachTypeToSymVar(_))
        mth.args.map(attachTypeToSymFormal(_))
        mth.getSymbol.retType = makeType(mth.retType)
      })
      cl.getSymbol.setType(new TClass(cl.getSymbol))
    })
    prog.main.vars.map(attachTypeToSymVar(_))



    /* check method overriding formals match*/

    
    prog.classes.map(cl => {
      cl.methods.map(mth => {
        if(mth.overrides)
          if(mth.getSymbol.argList.length == mth.getSymbol.overridden.get.argList.length){
            if(!(mth.getSymbol.argList zip mth.getSymbol.overridden.get.argList map {case (left, right) => left.getType == right.getType} forall Predef.identity))
              Reporter.error("argumets have to match when overriding", mth)
            if(mth.getSymbol.retType != mth.getSymbol.overridden.get.retType)
              Reporter.error("return types must match when overriding", mth.retType)
          }
          else{
            Reporter.error("arguments have to match when overriding", mth)
          }
      })
    })

    /**/
    prog
  }
}
