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

    /*val main_name= (ctx.file match {
      case Some(f) => f.getName().split('.').head
      case None    => "Main" 
    })*/
    val main_name="Main"

    def makeType(tp:TypeTree):String={
      tp match {
        case t:BooleanType  => "Z"
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
        case TUnit    => "V";
        case x:TClass => "L" + x.name + ";";
        case v        => throw new RuntimeException("This could only be TInt, TString or TBoolean.");
      }
    }
    
    def onTypeDo(tp:Type, ifun:() => Unit, bfun:() => Unit, sfun:() => Unit, cfun:() => Unit):Unit = {
      tp match {
        case TInt     => ifun();
        case TBoolean => bfun();
        case TString  => sfun();
        case _:TClass => cfun();
        case _:TBottom => cfun();
        case x        => throw new RuntimeException ("WTF? this should not happen, something went worng in type checking, recived unvalid type");
      } 
    } 
    
    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val classFile = new ClassFile(ct.id.value, ct.parent map (x => x.value))
      classFile.setSourceFile(ct.file.getPath)
      val dcch = classFile.addConstructor(Nil).codeHandler
      dcch << ALOAD_0;
      
      val parname = ct.getSymbol.parent match{
        case None => "java/lang/Object"
        case Some(parSym) => parSym.name
      }
      dcch<< InvokeSpecial(parname, "<init>", "()V")

      ct.vars map (x => {
        classFile.addField( makeType(x.tpe), x.id.value)
        def l(y:String): Int = 0;
        dcch << ALOAD_0;
        generateExpr(dcch, x.expr, l , ct.getSymbol )
        dcch << PutField(ct.id.value, x.id.value,
                         typeToBCType(x.id.getSymbol.getType));
      });
      dcch<< RETURN
      dcch.freeze
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
      mt.vars map (va => generateExpr(ch, new Assign(va.id, va.expr), slotFor, methSym))
      mt.exprs  map (generateExprOuter(ch, _, slotFor, methSym))
      generateExpr(ch, mt.retExpr,slotFor,methSym); 
      mt.retType match {
        case t:BooleanType  => ch << IRETURN
        case t:IntType      => ch << IRETURN
        case t:StringType   => ch << ARETURN
        case t:UnitType     => ch << RETURN
        case t:Identifier   => ch << ARETURN
      }

      ch.freeze
    }

    def generateExprOuter(ch: CodeHandler,  expr: ExprTree, slotFor:(String => Int), methSym: Symbol){
      generateExpr(ch, expr, slotFor, methSym)
      expr.getType match {
        case TInt     =>  ch << POP;
        case TBoolean =>  ch << POP;
        case TString  =>  ch << POP;
        case x:TClass =>  ch << POP;
        case x:TBottom =>  ch << POP;
        case v        =>  {};
      }
    }
    def generateExpr(ch: CodeHandler,  expr: ExprTree, slotFor:(String => Int), methSym: Symbol){
      expr match{
        case v:And        =>  generateExpr(ch, new If(v.lhs, v.rhs, Some(new False) ), slotFor, methSym);
        case v:Or         =>   generateExpr(ch, new If(v.lhs, new True, Some(v.rhs) ), slotFor, methSym);
        case v:Plus       => { 
          if(v.lhs.getType==TInt && v.rhs.getType==TInt){
            generateExpr(ch, v.lhs, slotFor, methSym);
            generateExpr(ch, v.rhs, slotFor, methSym);
            ch << IADD;
          } else if (v.lhs.getType == TString || v.rhs.getType == TString){
            ch << DefaultNew("java/lang/StringBuilder");
            generateExpr(ch, v.lhs, slotFor, methSym);
            
            ch << InvokeVirtual("java/lang/StringBuilder",
                                "append", v.lhs.getType match{
                                  case TInt        => "(I)Ljava/lang/StringBuilder;";
                                  case TString     => "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
                                  case TBoolean    => "(Z)Ljava/lang/StringBuilder;";
                                  case v           => throw new RuntimeException("WTF? this should not happen,"+
                                    "type checker should have thrown an error if one of operands to plus is not int string or bool");
                                })
            generateExpr(ch, v.rhs, slotFor, methSym);
            ch << InvokeVirtual("java/lang/StringBuilder",
                               "append", v.rhs.getType match{
                                  case TInt        => "(I)Ljava/lang/StringBuilder;";
                                  case TString     => "(Ljava/lang/String;)Ljava/lang/StringBuilder;";
                                  case TBoolean    => "(Z)Ljava/lang/StringBuilder;";
                                  case v           => throw new RuntimeException("WTF? this should not happen,"+
                                    " type checker should have thrown an error if one of operands to plus is not int string or bool");
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
        case v:Equals     => {
          if( (v.lhs.getType==TInt && v.rhs.getType==TInt)||
              (v.lhs.getType==TBoolean && v.rhs.getType==TBoolean) ){
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
          }else if ((v.lhs.getType==TString && v.rhs.getType==TString) || 
                    (!v.lhs.getType.primitive && !v.rhs.getType.primitive)) {
            val lTrue = ch.getFreshLabel("truelabel");
            val lEnd = ch.getFreshLabel("endlabel");
            generateExpr(ch, v.lhs, slotFor, methSym);
            generateExpr(ch, v.rhs, slotFor, methSym);
            ch << If_ACmpEq(lTrue);
              ch << ICONST_0;
              ch << Goto(lEnd);
            ch << Label(lTrue);
              ch << ICONST_1;
            ch << Label(lEnd);
          }else{
            throw new RuntimeException("WTF? this sholud not happen, types are mismatched,\n the type checker should have caught this")
          }
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
        case v:Identifier =>{
          methSym match{
            case methsymbol:MethodSymbol => v.getSymbol match {
              case varSym:VariableSymbol => {
                 if(methsymbol.isLocalVar(v.value)){
                    onTypeDo(v.getSymbol.getType,
                             ()=>ch << ILoad(slotFor(v.getSymbol.name)),
                             ()=>ch << ILoad(slotFor(v.getSymbol.name)),
                             ()=>ch << ALoad(slotFor(v.getSymbol.name)),
                             ()=>ch << ALoad(slotFor(v.getSymbol.name))
                         )
                  } else if(methsymbol.isArg(v.value)){
                    ch << ArgLoad(methsymbol.params.keys.toArray.indexOf(v.value)+1);
                  } else if(methsymbol.isField(v.value)){
                    ch << ALOAD_0
                    ch << GetField(methsymbol.classSymbol.name, v.value,  typeToBCType(v.getSymbol.getType));
                  }
              }
              case notVarsym => throw new RuntimeException ("WTF? something went worng in name analysis or type checking,"+
                                                            "indentefiers in expressions must be variable names and have  have a var symbol attached");
            }
            case classSymbol:ClassSymbol => v.getSymbol match {
              case varSym:VariableSymbol => 
                 // GetField(classSymbol.name, v.value,typeToBCType(v.getSymbol.getType));
                   onTypeDo(v.getSymbol.getType,
                             ()=>ch << ILoad(slotFor(v.getSymbol.name)),
                             ()=>ch << ILoad(slotFor(v.getSymbol.name)),
                             ()=>ch << ALoad(slotFor(v.getSymbol.name)),
                             ()=>ch << ALoad(slotFor(v.getSymbol.name))
                         )

              case notVarsym => throw new RuntimeException ("WTF? something went worng in name analysis or type checking,"+ 
                                                          "indentefiers in expressions must be variable names and have  have a var symbol attached");
            }
          }
        }
        case v:This       => methSym match {
            case methsymbol:MethodSymbol => ch << ArgLoad(0)
            case _                       => Reporter.error("Cant use access this in main method", v)

        }
        case v:Null       => ch << ACONST_NULL;
        case v:New        => ch << DefaultNew(v.tpe.value);
        case v:Not        => //generateExpr(ch,v.expr,slotFor, methSym);
                             //ch << INEG;
                            generateExpr(ch, new If(v.expr, new False, Some(new True)),slotFor,methSym)
        case v:Block      => //v.exprs map (x => generateExprOuter(ch, x, slotFor, methSym));
                               if(v.exprs.length <= 1)
                                v.exprs map (x => generateExpr(ch, x, slotFor, methSym));
                              else{
                                v.exprs.dropRight(1) map (x => generateExprOuter(ch, x, slotFor, methSym));
                                generateExpr(ch, v.exprs.last, slotFor,methSym); 
                              }
        case v:If         => { 
          val afterThis = ch.getFreshLabel("afterThis")
          val afterIf   = ch.getFreshLabel("afterIf")
          generateExpr(ch,  v.expr, slotFor, methSym);
          ch << ICONST_0;
          ch << If_ICmpEq(afterThis)
          generateExpr(ch,v.thn ,slotFor, methSym);
          ch << Goto(afterIf)
          ch << Label(afterThis)
          v.els match {
            case None => 
            case Some(elsExpr) => generateExpr(ch, elsExpr ,slotFor, methSym);
          }
          ch<< Label(afterIf)
        }
        case v:While      => {
          val beforeWhile = ch.getFreshLabel("beforeWhile")
          val afterWhileBody  = ch.getFreshLabel("afterWhileBody")
          ch << Label(beforeWhile)
          generateExpr(ch, v.cond , slotFor, methSym);
          ch << ICONST_0
          ch << If_ICmpEq(afterWhileBody)
          generateExpr(ch,v.body ,slotFor, methSym);
          ch << Goto (beforeWhile)
          ch << Label (afterWhileBody)
        }
        case v:Println    => {
                ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;");
                v.expr.getType match {
                  case TString  =>  generateExpr(ch, v.expr, slotFor, methSym);
                                    ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V");
                  case TInt     =>  //ch << DefaultNew("Ljava/lang/StringBuilder;");
                                    generateExpr(ch, v.expr, slotFor, methSym);
                                    //ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/String;")
                                    //ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
                                    ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V");
                  case TBoolean =>  //ch << DefaultNew("Ljava/lang/StringBuilder;");
                                    generateExpr(ch, v.expr, slotFor, methSym);
                                    //ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Z)Ljava/lang/String;")
                                    //ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;");
                                    ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V");
                  case other    => throw new RuntimeException ("WTF? somethnig went wrong in the type checker, input to printline must be string, int or boolean!")
                }
        }
        case v:Assign     => {
          v.id.getSymbol match {
            case varSym:VariableSymbol => { 
              methSym match{
                case methSymbol:MethodSymbol =>{
                  if(methSymbol.isLocalVar(v.id.value)){
                    generateExpr(ch,v.expr ,slotFor, methSym);
                    onTypeDo(v.id.getSymbol.getType,
                             ()=> ch << IStore(slotFor(v.id.getSymbol.name)),
                             ()=> ch << IStore(slotFor(v.id.getSymbol.name)),
                             ()=> ch << AStore(slotFor(v.id.getSymbol.name)),
                             ()=> ch << AStore(slotFor(v.id.getSymbol.name))
                             )
                  } else if(methSymbol.isArg(v.id.value)){
                    Reporter.error("cant reasign an argument", v);
                  } else if(methSymbol.isField(v.id.value)){
                    ch << ALOAD_0 // load this object  on the stack
                    generateExpr(ch,v.expr ,slotFor, methSym);
                    ch << PutField(methSymbol.classSymbol.name, v.id.value, typeToBCType(v.id.getSymbol.getType));
                  }else{

                  }
                }
                case classSym:ClassSymbol => {
                  generateExpr(ch,v.expr ,slotFor, methSym);
                  onTypeDo(v.id.getSymbol.getType,
                             ()=> {ch << IStore(slotFor(v.id.getSymbol.name))},
                             ()=> {ch << IStore(slotFor(v.id.getSymbol.name))},
                             ()=> {ch << AStore(slotFor(v.id.getSymbol.name))},
                             ()=> {ch << AStore(slotFor(v.id.getSymbol.name))}
                             )
                  //PutField(classSym.name, v.id.value, typeToBCType(v.id.getSymbol.getType));
                }
              }
            }
            case notVarsym  => throw new RuntimeException ("WTF? something went worng in name analysis or type checking,"+ 
                                                               "indentefiers in expressions must be variable names and have  have a var symbol attached");
        }
      }
      
      case v => throw new RuntimeException ("WTF? bad input to this function, the input to this function should be an expression tree");
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
    val mainClassFile =  new ClassFile(main_name, None)
    val mainCH = mainClassFile.addMainMethod.codeHandler
    mainClassFile.addDefaultConstructor
    var slots = Map[String, Int]();
    def slotFor(name: String): Int = {
      slots get name match{
        case None => {
          val r = mainCH.getFreshVar;
          slots += (name -> r);
          r;
        }
        case Some(s) => s
      }
    }
    prog.main.vars map (va =>{
      generateExpr(mainCH, new Assign(va.id, va.expr), slotFor, prog.main.getSymbol)
    })
    prog.main.exprs  map (generateExprOuter(mainCH, _, slotFor, prog.main.getSymbol))
    mainCH << RETURN
    mainCH.freeze
    mainClassFile.writeToFile(outDir+main_name+".class" );


  }

}
