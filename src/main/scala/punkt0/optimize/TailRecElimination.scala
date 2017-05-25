package punkt0
package optimize

import ast.Trees._
import analyzer._
import analyzer.Symbols._


object TailRecElimination extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._
    // Step 1: find tail recursive methods

    def isCallExpression(expr:ExprTree, meth:MethodSymbol):Boolean = expr match {
        case v:And        =>  false
        case v:Or         =>  false
        case v:Plus       =>  false
        case v:Minus      =>  false
        case v:Times      =>  false
        case v:Div        =>  false
        case v:LessThan   =>  false
        case v:Equals     =>  false
        case v:MethodCall =>  v.meth.getSymbol.id == meth.id
        case v:IntLit     =>  false
        case v:StringLit  =>  false
        case v:True       =>  false
        case v:False      =>  false
        case v:Identifier =>  false
        case v:This       =>  false
        case v:Null       =>  false
        case v:New        =>  false
        case v:Not        =>  false
        case v:Block      =>  v.exprs.lastOption match {
            case None => false
            case Some(innerExpr) => isCallExpression(innerExpr, meth)
        }
        case v:If         => isCallExpression(v.thn, meth) || (v.els match {
            case None=> false
            case Some(innerExpr) => isCallExpression(innerExpr, meth)
        })
        case v:While      =>  false
        case v:Println    =>  false
        case v:Assign     =>  false
    }

    //Step 2: Start rewriting
    var arg_to_sym_symbols = Map[VariableSymbol, VariableSymbol]()
    def change_expresion(in:ExprTree):ExprTree = in match {
        case v:And        =>  new And(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType)
        case v:Or         =>  new Or(change_expresion(v.lhs), change_expresion(v.rhs))
        case v:Plus       =>  new Plus(change_expresion(v.lhs), change_expresion(v.rhs)) 
        case v:Minus      =>  new Minus(change_expresion(v.lhs), change_expresion(v.rhs))
        case v:Times      =>  new Times(change_expresion(v.lhs), change_expresion(v.rhs))
        case v:Div        =>  new Div(change_expresion(v.lhs), change_expresion(v.rhs))
        case v:LessThan   =>  new LessThan(change_expresion(v.lhs), change_expresion(v.rhs)) 
        case v:Equals     =>  new Equals(change_expresion(v.lhs), change_expresion(v.rhs))
        case v:MethodCall =>  v 
        case v:IntLit     =>  v
        case v:StringLit  =>  v 
        case v:True       =>  v  
        case v:False      =>  v 
        case v:Identifier =>{ 
            v.getSymbol match {
                case sym:VariableSymbol => (arg_to_sym_symbols get sym) match {
                     case None   => v
                     case Some(x)=> new Identifier(x.name).setSymbol(x)
                }                              
                case _ => v
            }
        }
        case v:This       =>  v
        case v:Null       =>  v
        case v:New        =>  v        
        case v:Not        =>  new Not(change_expresion(v.expr))
        case v:Block      =>  new Block( v.exprs map ( x=> change_expresion(x)))
        case v:If         =>  new If(change_expresion(v.expr), change_expresion(v.thn), v.els map (x=> change_expresion(x)))
        case v:While      =>  new While( change_expresion(v.cond), change_expresion(v.body))
        case v:Println    =>  new Println(change_expresion(v.expr))
        case v:Assign     =>  new Assign(v.id, change_expresion(v.expr)) 
    }
    def change_retExpr(in:ExprTree):ExprTree =  {
        change_expresion(in)
    }

    println("###################################################")
    println("###################################################")
    println("###################################################")
    println("###################################################")
    println("###################################################")
    println("###################################################")
    println("###################################################")
    println("###################################################")
    println("###################################################")
    println("###################################################")
    println("###################################################")
    new Program(prog.main,  
        prog.classes.map( cls => {
         new ClassDecl(cls.id, cls.parent, cls.vars, cls.methods.map( mth => {
            // introduce variables
            var arg_to_sym = Map[String, String]()
            new MethodDecl(mth.overrides, mth.retType, mth.id, mth.args, 
                (mth.vars:::(mth.args map ( ar => {
                    val ourSym = new Symbols.VariableSymbol("_"+ar.id.value)
                    val ourVar = new VarDecl(ar.tpe, new Identifier("_"+ar.id.value), new Identifier(ar.id.value).setSymbol(ar.getSymbol) ).setSymbol(ourSym)
                    arg_to_sym += (  ar.id.value -> ("_" + ar.id.value + ourSym.id))
                    arg_to_sym_symbols += (  ar.getSymbol -> ourSym)
                    mth.getSymbol.members+=(arg_to_sym(ar.id.value) -> ourSym)
                    ourVar
                }))):+ 
                { 
                    val ourSym = new Symbols.VariableSymbol("_LOOP")
                    val ourVar = new VarDecl(new BooleanType(), new Identifier( "_LOOP"), new False() ).setSymbol(ourSym)
                    arg_to_sym += (  "_LOOP" -> "_LOOP")
                    mth.getSymbol.members+=("_LOOP"-> ourSym)
                    ourVar
                }, Nil,
                new Block((List( /*TODO: initilize variables, and _LOOP*/):::(mth.exprs map (x=>change_expresion(x)))):+ change_retExpr(mth.retExpr))
            )
         })).setSymbol(cls.getSymbol)
    })).setSymbol(prog.getSymbol) 
  }

}
