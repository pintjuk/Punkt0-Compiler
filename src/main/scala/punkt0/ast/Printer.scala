package punkt0
package ast

import Trees._

object Printer {
  var doIds=false;
  def endl(indent:Int ):String = "\n  "+"  "*indent
  def apply(t: Tree ):String = apply(t, 0)
  def apply(t: Tree, indent:Int ): String = {
    t match{
    	case v:Program => var str = v.classes.foldLeft("")((total, cur) => total + Printer(cur)) +  Printer(v.main); str;
    	case v:MainDecl =>	var firstExp:: exprs = v.exprs;
    						var str = "object " + Printer(v.obj) + 
                          " extends " + 
                          Printer(v.parent) + " {"+ endl(indent) + 
    						          v.vars.foldLeft("")( (total, cur) => total + Printer(cur)) +
                          endl(indent) + 
    						          exprs.foldLeft( Printer(firstExp, indent+1)) ( (total, cur) => total + ";\n" + Printer(cur,indent+1)) + " \n}"; str;
    	case v:ClassDecl => {
                var str = "class " + Printer(v.id);
    	          if(v.parent.isDefined){str += " extends " + Printer(v.parent.get) };
    						str += " {" +endl(indent);
    						for(va <- v.vars){str += Printer(va) + endl(indent)};
    						var first=true;
                for(meth <- v.methods){
                  if(first) first=false;
                  else str+=endl(indent);
                  str += Printer(meth, indent+1);
                }
    						str += "\n"+("  "*indent)+"}\n";
    						str};
    case v:VarDecl => var str = "var " + 
                                Printer(v.id) +
                                " : " + 
                                Printer(v.tpe) + 
                                " = " + 
                                Printer(v.expr) + 
                                ";"; str;
		case v:MethodDecl =>{var str= "";
    						  if(v.overrides){str = "override "};
    						  str +=  "def " + Printer(v.id) + "(";
    						  var temp = v.args.foldLeft("")((total, cur) =>total + Printer(cur) + ", "); 
    						  temp = temp.dropRight(2);
    						  str += temp;
    						  str += "): " + Printer(v.retType) + " = {" + endl(indent);
    						  str += v.vars.foldLeft("")((total, cur) => total + Printer(cur, indent)+endl(indent));
    						  for(ex <- v.exprs){str += Printer(ex, indent+1) + ";" +endl(indent)};
    						  str += Printer(v.retExpr) + "\n" +("  "*indent) +"}"; str};
    	case v:Formal => var str =  Printer(v.id) +  
                                  ": " + 
                                  Printer(v.tpe);
                       str;
  
    	case v:BooleanType => var str = "Boolean"; str;
    	case v:IntType => var str = "Int"; str;
    	case v:StringType => var str = "String"; str;
    	case v:UnitType => var str = "Unit"; str;
    	
    	case v:And => var str = "(" + Printer(v.lhs) + ")"  + " && " + "(" + Printer(v.rhs) + ")"; str;
    	case v:Or => var str = "(" + Printer(v.lhs) + ")" + " || " + "(" + Printer(v.rhs) + ")"; str;
      case v:Plus => var str = "(" + Printer(v.lhs) + ")" + " + " + "(" + Printer(v.rhs) + ")"; str;
	  	case v:Minus => var str = "(" + Printer(v.lhs) + ")" + " - " + "(" + Printer(v.rhs) + ")"; str;
    	case v:Times => var str = "(" + Printer(v.lhs) + ")" + " * " + "(" + Printer(v.rhs) + ")"; str;
    	case v:Div => var str = "(" + Printer(v.lhs) + ")" + " / " + "(" + Printer(v.rhs) + ")"; str;
    	case v:LessThan => var str = "(" + Printer(v.lhs) + ")" + " < " + "(" + Printer(v.rhs) + ")"; str;
    	case v:Equals => var str = "(" + Printer(v.lhs) + ")" + " == " + "(" + Printer(v.rhs) + ")"; str;
    	
    	case v:MethodCall => var str = Printer(v.obj) + "." + Printer(v.meth) + "(" +
                (v.args match{
                  case (h:ExprTree) :: ( tail:List[ExprTree] ) => tail.foldLeft(Printer(h))((total, cur) => total +", " + Printer(cur))
                  case List()   => ""
                }) + ")"; str;
    	case v:IntLit => var str = v.value.toString; str;
      case v:StringLit => var str = "\""+v.value+"\""; str;

    	case v:True => var str = "true"; str;
    	case v:False => var str = "false"; str;
    	case v:Identifier => var str = v.value +  ( if(doIds) "#" + v.symString
                                    else "" ); str;    	
    	case v:This => var str = "this"+  ( if(doIds) "#" + v.symString
                                    else "" ) ; str;
    	case v:Null => var str = "null"; str;
    	case v:New => var str = "new " + Printer(v.tpe) + "()"; str;
    	case v:Not => var str = "!" + "(" + Printer(v.expr) + ")"; str;
    	
    	case v:Block => {var str = (v.exprs match {
                          case List() => ""
                          case h::tail => tail.foldLeft(Printer(h, indent+1))((total, cur) => total + ";"+endl(indent) + Printer(cur, indent+1)  )
                        });
    	                str = "{"+endl(indent) + str + "\n"+("  "*indent)+"}";
    	                str;}
    	case v:If => {var str = "if (" + Printer(v.expr) + ") " + Printer(v.thn, indent);
    				  if(v.els.isDefined){ 
    				    str += "\n"+ ("  "*indent)+"else " + Printer(v.els.get, indent)
    				  }
              str+=" /*end if(" + Printer(v.expr) + ")*/";
    				  str;}
    	case v:While => var str = "while (" + Printer(v.cond) + ") " + Printer(v.body, indent) + " /* end while(" + Printer(v.cond) + ")*/"; str;
    	case v:Println => var str = "println(" + Printer(v.expr) + ")"; str;
    	case v:Assign => var str = Printer(v.id) + " = " + Printer(v.expr ); str;
    }
  }
}
