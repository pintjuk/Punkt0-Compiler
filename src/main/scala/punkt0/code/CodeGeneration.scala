package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._

object CodeGeneration extends Phase[Program, Unit] {

  def run(prog: Program)(ctx: Context): Unit = {


    def makeType(tp:TypeTree):String={
      tp match {
        case t:BooleanType  => "B"
        case t:IntType      => "I"
        case t:StringType   => "Ljava/lang/String;"
        case t:UnitType     => "V"
        case t:Identifier   => "L"+t.value+";"
      }
    }
    def typeToBCType(tp:Type):String={
      tp match {
        case TInt     => "I";
        case TBoolean => "Z";
        case TString  => "Ljava/lang/String;";
        case x:TClass => "L" + x.name + ";";
        case v        => throw new RuntimeException("This could only be TInt, TString or TBoolean.");
      }
    }
    
    def onTypeDo(tp:Type, ifun:() => Unit, bfun:() => Unit, sfun:() => Unit, cfun:() => Unit):Unit = {
      tp match {
        case TInt     => ifun();
        case TBoolean => bfun();
        case TString  => sfun();
        case x:TClass => cfun();
        case x        => throw new RuntimeException ("WTF? this should not happen, something went worng in type checking, recived unvalid type");
      } 
    } 
    
    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val classFile = new ClassFile(ct.id.value, ct.parent map (x => x.value))
      classFile.setSourceFile(ct.file.getPath)
      
      ct.vars map (x =>  classFile.addField( makeType(x.tpe), x.id.value));
      ct.methods map ( mth => 
          generateMethodCode( 
            classFile.addMethod(
              makeType(mth.retType), 
              mth.id.value, 
              (mth.args map (arg => makeType(arg.tpe))):_* 
            ).codeHandler, 
            mth 
          ) 
        )
    
      classFile.writeToFile(dir + "/" + ct.id.value + ".class" );
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol;
      var slots = Map[String, Int]();
      def slotFor(name: String): Int = {
        slots get name match{
          case None => {
            val r = ch.getFreshVar;
            slots += (name -> r);
            r;
          }
          case Some(s) => s
        }
      }
      mt.exprs:::List(mt.retExpr) map (generateExpr(ch, _, slotFor, methSym))
      
      mt.retType match {
        case t:BooleanType  => ch << IRETURN
        case t:IntType      => ch << IRETURN
        case t:StringType   => ch << ARETURN
        case t:UnitType     => ch << RETURN
        case t:Identifier   => ch << ARETURN
      }


      ch.freeze
    }

    def generateExpr(ch: CodeHandler,  expr: ExprTree, slotFor:(String => Int), methSym: MethodSymbol){
      expr match{
        case v:And        => 
        case v:Or         =>  
        case v:Plus       => { 
                             if(v.lhs.getType==TInt && v.rhs.getType==TInt){
                                generateExpr(ch, v.lhs, slotFor, methSym);
                                generateExpr(ch, v.rhs, slotFor, methSym);
                                ch << IADD;
                              } else if (v.lhs.getType == TString || v.rhs.getType == TString){
                                ch << DefaultNew("Ljava/lang/StringBuilder;");
                                generateExpr(ch, v.lhs, slotFor, methSym);
                                
                                ch << InvokeVirtual("java/lang/StringBuilder",
                                                    "append", v.lhs.getType match{
                                                      case TInt        => "(I)Ljava/lang/String;";
                                                      case TString     => "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
                                                      case TBoolean    => "(Z)Ljava/lang/String;";
                                                      case v           => throw new RuntimeException("WTF? this should not happen, type checker should have thrown an error if one of operands to plus is not int string or bool");
                                                    })
                                generateExpr(ch, v.rhs, slotFor, methSym);
                                ch << InvokeVirtual("java/lang/StringBuilder",
                                                   "append", v.rhs.getType match{
                                                      case TInt        => "(I)Ljava/lang/String;";
                                                      case TString     => "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
                                                      case TBoolean    => "(Z)Ljava/lang/String;";
                                                      case v           => throw new RuntimeException("WTF? this should not happen, type checker should have thrown an error if one of operands to plus is not int string or bool");
                                                    })
                                ch << InvokeVirtual("java/lang/StringBuilder",
                                                    "toString",
                                                    "()Ljava/lang/String;");
                              }
                            }
        case v:Minus      =>  generateExpr(ch, v.lhs, slotFor, methSym); 
                              generateExpr(ch, v.rhs, slotFor, methSym); 
                              ch << ISUB;  
        case v:Times      =>  generateExpr(ch, v.lhs, slotFor, methSym);
                              generateExpr(ch, v.rhs, slotFor, methSym);
                              ch << IMUL;  
        case v:Div        =>  generateExpr(ch, v.lhs, slotFor, methSym);
                              generateExpr(ch, v.rhs, slotFor, methSym);
                              ch << IDIV;  
        case v:LessThan   =>{
                              val lTrue = ch.getFreshLabel("truelabel");
                              val lEnd = ch.getFreshLabel("endlabel");
                              generateExpr(ch, v.lhs, slotFor, methSym);
                              generateExpr(ch, v.rhs, slotFor, methSym);
                              ch << If_ICmpLt(lTrue);
                                ch << ICONST_0;
                                ch << Goto(lEnd);
                              ch << Label(lTrue);
                                ch << ICONST_1;
                              ch << Label(lEnd);
                            }
        case v:Equals     => if(v.lhs.getType==TInt && v.rhs.getType==TInt)  
                            {
                             
                              val lTrue = ch.getFreshLabel("truelabel");
                              val lEnd = ch.getFreshLabel("endlabel");
                              generateExpr(ch, v.lhs, slotFor, methSym);
                              generateExpr(ch, v.rhs, slotFor, methSym);
                              ch << ISUB;
                              ch << IfEq(lTrue);
                                ch << ICONST_0;
                                ch << Goto(lEnd);
                              ch << Label(lTrue);
                                ch << ICONST_1;
                              ch << Label(lEnd);
                            }else if (v.lhs.getType==TBoolean && v.rhs.getType==TBoolean) {
                              //TODO boolean lazy compare 
                            }else if ((v.lhs.getType==TString && v.rhs.getType==TString) || 
                                      (v.lhs.getType.isInstanceOf[TClass] && v.rhs.getType.isInstanceOf[TClass])) {
                                        //TODO: compare by reference
                            }else{
                              throw new RuntimeException("WTF? this sholud not happen, types are mismatched,\n the type checker should have caught this")
                            }
        case v:MethodCall => {
                               generateExpr(ch, v.obj, slotFor, methSym);
                               v.args map (x => generateExpr(ch, x, slotFor, methSym));
                               v.meth.getSymbol match {
                                  case methsym:MethodSymbol => ch << InvokeVirtual(methsym.classSymbol.name, 
                                                               v.meth.getSymbol.name,
                                                               "(" + (methsym.argList map (x => typeToBCType(x.getType))).mkString("") +
                                                               ")" + typeToBCType(methsym.retType))
                                  case notmethsym           => throw new RuntimeException ("WTF? something went worng in name analysis or type checking, methCall should have a method symbol attached");
                               }
                             }
        case v:IntLit     => ch << Ldc(v.value);
        case v:StringLit  => ch << Ldc(v.value);
        case v:True       => ch << ICONST_1;
        case v:False      => ch << ICONST_0;
        case v:Identifier => v.getSymbol match {
                                case varSym:VariableSymbol => 
                                              if(methSym.isLocalVar(v.value)){
                                                  onTypeDo(v.getSymbol.getType,
                                                           ()=> ILoad(slotFor(v.getSymbol.name)),
                                                           ()=> ILoad(slotFor(v.getSymbol.name)),
                                                           ()=> ALoad(slotFor(v.getSymbol.name)),
                                                           ()=> ALoad(slotFor(v.getSymbol.name))
                                                       )
                                                } else if(methSym.isArg(v.value)){
                                                  ArgLoad(methSym.params.keys.toArray.indexOf(v.value)+1);
                                                } else if(methSym.isField(v.value)){
                                                  ()=> GetField(methSym.classSymbol.name,
                                                                v.value,
                                                                typeToBCType(v.getSymbol.getType));
                                                }
                                case notVarsym            => throw new RuntimeException ("WTF? something went worng in name analysis or type checking, indentefiers in expressions must be variable names and have  have a var symbol attached");
                             }
        case v:This       => ch << ArgLoad(0);
        case v:Null       => ch << ACONST_NULL;
        case v:New        => ch << DefaultNew("L"+v.tpe.value+";");
        case v:Not        => generateExpr(ch,v.expr,slotFor, methSym);
                             ch << INEG;
        case v:Block      => v.exprs map (x => generateExpr(ch, x, slotFor, methSym));
        case v:If         => 
        case v:While      =>  
        case v:Println    => {
                              ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;");
                              v.expr.getType match {
                                case TString  =>  generateExpr(ch, v.expr, slotFor, methSym);
                                case TInt     =>  ch << DefaultNew("Ljava/lang/StringBuilder;");
                                                  generateExpr(ch, v.expr, slotFor, methSym);
                                                  ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/String;")
                                case TBoolean =>  ch << DefaultNew("Ljava/lang/StringBuilder;");
                                                  generateExpr(ch, v.expr, slotFor, methSym);
                                                  ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Z)Ljava/lang/String;")
                                case other    => throw new RuntimeException ("WTF? somethnig went wrong in the type checker, input to printline must be string, int or boolean!")
                              }
                              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
                              ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V");
                            }
        case v:Assign     =>
        case v            => throw new RuntimeException ("WTF? bad input to this function, the input to this function should be an expression tree");
      }

    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main declaration
    // ...
  }

}
