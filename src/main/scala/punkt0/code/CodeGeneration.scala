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
      tp match{
        case t:BooleanType  => "B"
        case t:IntType      => "I"
        case t:StringType   => "Ljava/lang/String;"
        case t:UnitType     => "V"
        case t:Identifier   => "L"+t.value+";"
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
      mt.exprs:::List(mt.retExpr) map (generateExpr(ch, _, slotFor))
      
      mt.retType match {
        case t:BooleanType  => ch << IRETURN
        case t:IntType      => ch << IRETURN
        case t:StringType   => ch << ARETURN
        case t:UnitType     => ch << RETURN
        case t:Identifier   => ch << ARETURN
      }


      ch.freeze
    }

    def generateExpr(ch: CodeHandler,  expr: ExprTree, slotFor:(String => Int)){
      expr match{
        case v:And        => 
        case v:Or         =>
        case v:Plus       =>
        case v:Minus      =>  generateExpr(ch, v.lhs, slotFor); 
                              generateExpr(ch, v.rhs, slotFor); 
                              ch << ISUB;  
        case v:Times      =>  generateExpr(ch, v.lhs, slotFor);
                              generateExpr(ch, v.rhs, slotFor);
                              ch << IMUL;  
        case v:Div        =>  generateExpr(ch, v.lhs, slotFor);
                              generateExpr(ch, v.rhs, slotFor);
                              ch << IDIV;  
        case v:LessThan   =>{
                              val lTrue = ch.getFreshLabel("truelabel");
                              val lEnd = ch.getFreshLabel("endlabel");
                              generateExpr(ch, v.lhs, slotFor);
                              generateExpr(ch, v.rhs, slotFor);
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
                              generateExpr(ch, v.lhs, slotFor);
                              generateExpr(ch, v.rhs, slotFor);
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
        case v:MethodCall =>  
        case v:IntLit     => ch << Ldc(v.value);
        case v:StringLit  => ch << Ldc(v.value);
        case v:True       => ch << ICONST_1;
        case v:False      => ch << ICONST_0;
        case v:Identifier => 
        case v:This       => 
        case v:Null       => ch << ACONST_NULL;
        case v:New        => ch << DefaultNew("L"+v.tpe.value+";");
        case v:Not        => generateExpr(ch,v.expr,slotFor);
                             ch << INEG;
        case v:Block      => v.exprs map (x => generateExpr(ch, x, slotFor));
        case v:If         => 
        case v:While      =>  
        case v:Println    => 
        case v:Assign     => 
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
