package punkt0
package optimize

import ast.Trees._
import analyzer._
import analyzer.Symbols._
import analyzer.Types._


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
    var arg_to_sym_symbols = Map[Int, VariableSymbol]()
    var arg_to_sym = Map[String, String]()
    def change_expresion(in:ExprTree):ExprTree = in match {
        case v:And        =>  new And(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType).setPos(v)
        case v:Or         =>  new Or(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType).setPos(v)
        case v:Plus       =>  new Plus(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType).setPos(v)
        case v:Minus      =>  new Minus(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType).setPos(v)
        case v:Times      =>  new Times(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType).setPos(v)
        case v:Div        =>  new Div(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType).setPos(v)
        case v:LessThan   =>  new LessThan(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType).setPos(v)
        case v:Equals     =>  new Equals(change_expresion(v.lhs), change_expresion(v.rhs)).setType(v.getType).setPos(v)
        case v:MethodCall =>  new MethodCall(change_expresion(v.obj), v.meth, 
                                                v.args map (argument => change_expresion(argument))
                                            ).setType(v.getType).setPos(v)
        case v:IntLit     =>  v
        case v:StringLit  =>  v 
        case v:True       =>  v 
        case v:False      =>  v
        case v:Identifier =>{
            v.getSymbol match {
                case sym:VariableSymbol => 
                    (arg_to_sym_symbols get sym.id) match {
                     case None   => v
                     case Some(x)=> new Identifier(x.name).setSymbol(x).setType(v.getType).setPos(v)
                }                              
                case _ => v
            }
        }
        case v:This       =>  v
        case v:Null       =>  v
        case v:New        =>  v        
        case v:Not        =>  new Not(change_expresion(v.expr)).setType(v.getType).setPos(v)
        case v:Block      =>  new Block( v.exprs map ( x=> change_expresion(x))).setType(v.getType).setPos(v)
        case v:If         =>  new If(change_expresion(v.expr), change_expresion(v.thn), v.els map (x=> change_expresion(x))).setType(v.getType).setPos(v)
        case v:While      =>  new While( change_expresion(v.cond), change_expresion(v.body)).setType(v.getType).setPos(v)
        case v:Println    =>  new Println(change_expresion(v.expr)).setType(v.getType).setPos(v)
        case v:Assign     =>  new Assign(v.id, change_expresion(v.expr)).setType(v.getType).setPos(v)
    }
    def change_tailcall_return(in:ExprTree, meth:MethodSymbol):ExprTree ={
        def change_ret (expr:ExprTree): ExprTree ={
            val sym = meth.members("_RES")
            new Assign(new Identifier("_RES").setSymbol(sym).setType(sym.getType), expr) 
        }
        in match {
        case v:And        => change_ret(v) 
        case v:Or         => change_ret(v) 
        case v:Plus       =>change_ret(v) 
        case v:Minus      =>change_ret(v)
        case v:Times      =>change_ret(v)
        case v:Div        =>change_ret(v)
        case v:LessThan   =>change_ret(v)
        case v:Equals     => change_ret(v)
        case v:MethodCall =>  if (v.meth.getSymbol.id == meth.id){
                                 new Block({
                                     var i=0
                                     var assigns:List[ExprTree] = Nil
                                     for(i<- 0 until v.args.size){
                                         var symbol = arg_to_sym_symbols(meth.argList(i).id)
                                         assigns = assigns:+ (new Assign( new Identifier( symbol.name).setSymbol(symbol).setType(symbol.getType), v.args(i)))
                                     }
                                     var loop_id = new Identifier("_LOOP").setSymbol(meth.members("_LOOP")).setType(TBoolean)
                                     assigns:+new Assign (loop_id, new True())
                                  }).setType(TUnit)
                              } 
                              else{
                                 change_ret(v)
                              }
        case v:IntLit     =>  change_ret(v)
        case v:StringLit  =>  change_ret(v) 
        case v:True       =>  change_ret(v) 
        case v:False      =>  change_ret(v)
        case v:Identifier =>  change_ret(v)
        case v:This       =>  change_ret(v)
        case v:Null       =>  change_ret(v)
        case v:New        =>  change_ret(v)        
        case v:Not        =>  change_ret(v) 
        case v:Block      =>   v.exprs.lastOption match {
            case None=> v
            case Some(x) => new Block(v.exprs.updated(v.exprs.size-1, change_tailcall_return(x, meth))).setType(v.getType).setPos(v)
        }
        case v:If         =>  new If(v.expr,  change_tailcall_return(v.thn, meth), v.els map (x=> change_tailcall_return(x, meth))).setType(v.getType).setPos(v)
        case v:While      =>  change_ret(v)
        case v:Println    =>  change_ret(v)
        case v:Assign     =>  change_ret(v)
        }
    }
    def change_retExpr(in:ExprTree):ExprTree =  {
        change_expresion(in)
    }

new Program(prog.main,  
        prog.classes.map( cls => {
         new ClassDecl(cls.id, cls.parent, cls.vars, cls.methods.map( mth => if( isCallExpression( mth.retExpr, mth.getSymbol )) {
            // introduce variables
            val reassign_vars = mth.vars map( ar => {
                new Assign (ar.id, ar.expr)
            })
            new MethodDecl(mth.overrides, mth.retType, mth.id, mth.args, 
                /* variables */
                (mth.vars:::(mth.args map ( ar => {
                    val ourSym = new Symbols.VariableSymbol("_"+ar.id.value).setType(ar.getSymbol.getType)
                    val ourVar = new VarDecl(ar.tpe, new Identifier("_"+ar.id.value).setSymbol(ourSym).setType(ar.getSymbol.getType)
                        , new Identifier(ar.id.value).setSymbol(ar.getSymbol) ).setSymbol(ourSym)
                    arg_to_sym += (  ar.id.value -> ("_" + ar.id.value))
                    arg_to_sym_symbols += (  ar.getSymbol.id -> ourSym)
                    mth.getSymbol.members+=(arg_to_sym(ar.id.value) -> ourSym)
                    ourVar
                }))):+ 
                { 
                    val ourSym = new Symbols.VariableSymbol("_LOOP").setType(TBoolean)
                    val ourVar = new VarDecl(new BooleanType(), new Identifier( "_LOOP").setSymbol(ourSym).setType(TBoolean) , new True() ).setSymbol(ourSym)
                    arg_to_sym += (  "_LOOP" -> "_LOOP")
                    mth.getSymbol.members+=("_LOOP"-> ourSym)
                    ourVar
                }:+{
                    val rType=mth.retExpr.getType;
                    val ourSym = new Symbols.VariableSymbol("_RES").setType(rType)
                    val ourVar = new VarDecl(mth.retType, new Identifier( "_RES").setSymbol(ourSym).setType(rType) , rType match {
                        case TBoolean => new False()
                        case TString  => new StringLit("")
                        case TInt     => new IntLit(0)
                        case x:TClass => new Null()
                    }).setSymbol(ourSym)
                    arg_to_sym += (  "_RES" -> "_RES")
                    mth.getSymbol.members+=("_RES"-> ourSym)
                    ourVar
                }, List({
                        var loop_id = new Identifier("_LOOP").setSymbol(mth.getSymbol.members("_LOOP")).setType(TBoolean)
                        new While( loop_id,
                            new Block(( new Assign(loop_id, new False()) :: reassign_vars :::
                            (mth.exprs map (x=>change_expresion(x)))):+ 
                            change_tailcall_return( change_retExpr(mth.retExpr), mth.getSymbol )).setPos(mth.retExpr).setType(TUnit)
                        ).setPos(mth.retExpr).setType(TUnit)
                    })
                ,
                /* return expersion */
                new Identifier("_RES").setSymbol(mth.getSymbol.members("_RES"))
            ).setSymbol(mth.getSymbol).setPos(mth)
         }
         else {
             mth
         }
         )).setSymbol(cls.getSymbol).setPos(cls)
    })).setSymbol(prog.getSymbol).setPos(prog) 
  }

}
