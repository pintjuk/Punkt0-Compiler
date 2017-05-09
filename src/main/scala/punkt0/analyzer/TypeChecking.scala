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

    def tcExpr(expr: ExprTree,scope:Scope, expected: Type*): Type = {
      val tpe: Type = expr match{
        case v:And        =>  tcExpr(v.lhs,scope, TBoolean); tcExpr(v.rhs,scope, TBoolean);
        case v:Or         =>  tcExpr(v.lhs,scope, TBoolean); tcExpr(v.rhs,scope, TBoolean);
        case v:Plus       =>  val lt = tcExpr(v.lhs,scope, TInt, TString); 
                              val rt = tcExpr(v.rhs,scope, TInt, TString); 
                              if(rt==TInt && lt==TInt)  TInt 
                              else                      TString;
        case v:Minus      =>  tcExpr(v.lhs,scope, TInt); tcExpr(v.rhs,scope, TInt);
        case v:Times      =>  tcExpr(v.lhs,scope, TInt); tcExpr(v.rhs,scope, TInt);
        case v:Div        =>  tcExpr(v.lhs,scope, TInt); tcExpr(v.rhs,scope, TInt);
        case v:LessThan   =>  tcExpr(v.lhs,scope, TInt); tcExpr(v.rhs,scope, TInt); TBoolean
        case v:Equals     =>  val lt = tcExpr(v.lhs,scope);
                              val rt = tcExpr(v.rhs,scope);
                              if (!(lt==rt) && (lt.primitive || rt.primitive )) { 
                                Reporter.error("Type error: type missmatch, not type correct to compare " + rt+" and "+ lt, expr); 
                                TError;
                              }
                              else 
                                TBoolean
        case v:MethodCall =>  tcExpr(v.obj,scope, Types.anyRef) match{
                                case derefed:TClass => derefed.classSymbol.lookupMethod(v.meth.value) match {
                                  case None => {Reporter.error("Type error: object does not contain reference" + v.meth, v.obj); TError;}
                                  case Some(methSymbol) => {
                                    if( v.args.length == methSymbol.argList.length){
                                      v.args zip methSymbol.argList map {
                                        case (arg, param) => tcExpr(arg,scope, param.getType)
                                      }; 
                                      v.meth.setSymbol(methSymbol);
                                      methSymbol.retType;
                                    }else{
                                      Reporter.error("Arguments in method call must match arguments in method declaration", v)
                                      TError
                                    }
                                  }
                                }
                                case tp => {
                                  Reporter.error("Type error: cant dereference "+tp, v.obj)
                                  TError
                                }
                              }
        case v:IntLit     =>  TInt
        case v:StringLit  =>  TString
        case v:True       =>  TBoolean
        case v:False      =>  TBoolean
        case v:Identifier =>  v.getSymbol.getType
        case v:This       =>  v.getSymbol.getType
        case v:Null       =>  Types.bottomRef 
                              /*if(expected.isEmpty) Types.anyRef
                              else if(expected.exists(e => e.isInstanceOf[TClass])) expected.filter(e => e.isInstanceOf[TClass]).head
                              else Reporter.error("Type error: this cant be Null", v); TError;*/
        case v:New        =>  v.tpe.getSymbol.getType
        case v:Not        =>  tcExpr(v.expr,scope, TBoolean)
        case v:Block      =>  v.exprs.slice(0, v.exprs.length-1).map(tcExpr(_,scope));
                              val res:Type =  if(v.exprs.length==0) TUnit 
                                              else tcExpr(v.exprs.last,scope, expected:_*);
                              res;
        case v:If         =>  tcExpr(v.expr,scope, TBoolean);
                              val thnT = tcExpr(v.thn,scope, expected:_*);
                              v.els match{
                                case None       => thnT;
                                case Some(els)  => {
                                  val elsT = tcExpr(els, scope, expected:_*); 
                                  try{
                                    elsT.lub(thnT)
                                  }catch{
                                    case primitivLubException =>{
                                      Reporter.error("Type error, one branches of if statment my not be " + 
                                                      thnT + " while the other is " + elsT, v.thn, "and", els)
                                      TError
                                    }
                                  }
                                }
                              }
        case v:While      =>  tcExpr(v.cond,scope, TBoolean); tcExpr(v.body,scope, TUnit);TUnit
        case v:Println    =>  tcExpr(v.expr,scope, TBoolean, TString, TInt); TUnit
        case v:Assign     =>  tcExpr(v.id, scope); scope.lookupVarNotArg(v.id.value) match{
                                case Some(varsymbol)  =>  tcExpr(v.expr,scope, v.id.getType);  TUnit
                                case None             =>  Reporter.error("Type error: Cannot assign to argument "+v.id.value, v);TError
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
      mth.vars.map(vr => tcExpr(vr.expr, mth.getSymbol, vr.getSymbol.getType))
      mth.exprs.map(tcExpr(_,mth.getSymbol));
      tcExpr(mth.retExpr, mth.getSymbol, expected:_*);
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
        if(mth.overrides){
          mth.getSymbol.overridden match {
            case Some(overriddenMethod)  => {
                if(mth.getSymbol.argList.length == overriddenMethod.argList.length){
                  if(!(mth.getSymbol.argList zip overriddenMethod.argList map {case (left, right) => left.getType == right.getType} forall Predef.identity))
                    Reporter.error("Type error: argumets have to match when overriding", mth)
                  if(mth.getSymbol.retType != mth.getSymbol.overridden.get.retType)
                    Reporter.error("Type error: return types must match when overriding", mth.retType)
                }
                else{
                  Reporter.error("Type error: arguments have to match when overriding", mth)
                }
            }
            case None => throw new IllegalStateException("WTF! MethdDecl says method overriddes but there is no overridden method in the symbol of " + mth.id.value);
          }
        }
      })
    })

    /* typecheck expressions add such */
    prog.classes.map(cl => {
      cl.vars.map(vr=> tcExpr(vr.expr,cl.getSymbol, vr.getSymbol.getType))
      cl.methods.map(mth=> tcMeth(mth, mth.getSymbol.retType))
    })

    prog.main.vars.map{ va=> tcExpr(va.expr, prog.main.getSymbol, va.getSymbol.getType)}
    prog.main.exprs.map(tcExpr(_,prog.main.getSymbol));
    prog

  }
}
