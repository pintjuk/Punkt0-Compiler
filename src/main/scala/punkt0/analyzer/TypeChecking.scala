package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {

    val globalScope = prog.getSymbol;

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match{
        case v:And        =>  tcExpr(v.lhs, TBoolean); tcExpr(v.rhs, TBoolean);
        case v:Or         =>  tcExpr(v.lhs, TBoolean); tcExpr(v.rhs, TBoolean);
        case v:Plus       =>  val lt = tcExpr(v.lhs, TInt, TString); 
                              val rt = tcExpr(rhs, TInt, TString); 
                              if(rt==Tint && lt==TInt)  TInt 
                              else                      TString;
        case v:Minus      =>  tcExpr(v.lhs, TInt); tcExpr(v.rhs, TInt);
        case v:Times      =>  tcExpr(v.lhs, TInt); tcExpr(v.rhs, TInt);
        case v:Div        =>  tcExpr(v.lhs, TInt); tcExpr(v.rhs, TInt);
        case v:LessThan   =>  tcExpr(v.lhs, TInt); tcExpr(v.rhs, TInt); TBoolean;
        case v:Equals     =>  val lt = tcExpr(v.lhs);
                              val rt = tcExpr(v.rhs);
                              if(lt==rt) TBoolean
                              else if(!(lt==rt) && lt.isInstanceOf[TClass]&& rt.isInstanceOf[TClass])
                              else error("Type error: type missmatch, cant compare primatives and classes", expr); TError;
        case v:MethodCall =>  tcExpr(v.obj, Types.anyRef).classSymbol.lookupMethod(v.meth) match {
                                case None => error("Type error: object does not contain reference" + v.meth, v.obj); TError;
                                case Some(methSymbol) v.args zip methSymbol.argList map {case (arg, param) => tcExpr(arg, param)}; methSymbol.retType;
                              }
        case v:IntLit     =>  TInt
        case v:StringLit  =>  TString
        case v:True       =>  TBoolean
        case v:False      =>  TBoolean
        case v:Identifier =>  v.getSymbol.getType
        case v:This       =>  v.getSymbol.getType
        case v:Null       =>  if(expected.isEmpty) Types.anyRef
                              else if(expected.exists(e => e.isInstanceOf[TClass])) expected.filter(e => e.isInstanceOf[TClass]).first   
                              else Report.error("Type error: this cant be Null", v); TError;
        case v:New        =>  v.tpe.getSymbol.getType
        case v:Not        =>  tcExpr(v.expr, TBoolean)
        case v:Block      =>  v.exprs.slice(0, v.exprs.length-2).map(tcExpr(_)); tcExpr(v.exprs.last, expected);
        case v:If         =>  tcExpr(v.expr, IBoolean);
                              val thnT = tcExpr(v.thn, expected);
                              v.els match{
                                case None       => thnT;
                                case Some(els)  => val elsT = tcExpr(els, expected);
                              }
      }

      expr.setType(tpe);


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
    
    def tcMeth(mth: MethodDecl, expected: Type*): Type = {
      mth.vars.map(vr => vr.setType = tcExpr(vr.expr, vr.getSymbol.getType))
      mth.exprs.map(tcExpr);
      tcExpr(mth.retExpr, expected);
    }

    


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
              Reporter.error("Type error: argumets have to match when overriding", mth)
            if(mth.getSymbol.retType != mth.getSymbol.overridden.get.retType)
              Reporter.error("Type error: return types must match when overriding", mth.retType)
          }
          else{
            Reporter.error("Type error: arguments have to match when overriding", mth)
          }
      })
    })

    /* typecheck expressions add such */
   prog.classes.map(cl => {
     cl.vars.map(vr=> vr.setType(tcExpr(vr.expr, vr.getSymbol.getType)))
     cl.methods.map(mth=> tcMeth(mth,mth.getSymbol.retType))
   })
    prog
  }
}
