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
      
      ct.vars map (x => val fn = classFile.addField( makeType(x.tpe), x.id.value))
      ct.methods map ( mth => 
          generateMethodCode( 
            classFile.addMethod(
              makeType(mth.retType), 
              mth.id.value, 
              (mth.args map (makeType(_.tpe))):_* 
            ).codeHandler, 
            mth 
          ) 
        )
      classFile.writeToFile(dir + "/" + ct.id.value + ".class");
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      var slots = Map[String, Int]();
      var slotFor = name => {
        slots get name match{
          case None => {
            val r = ch.getFreshVar;
            slots += (name -> r);
            r;
          }
          case Some(s) => s
        }
      }
      mt.exprs:::mt.retExpr map (generateExpr(ch, _, slotFor))
      
      mt.retType match {
        case t:BooleanType  => ch << IRETURN
        case t:IntType      => ch << IRETURN
        case t:StringType   => ch << ARETURN
        case t:UnitType     => ch << RETURN
        case t:Identifier   => ch << ARETURN
      }


      ch.freeze
    }

    def generateExpr(ch: CodeHandler,  expr: ExprTree, slotFor:(String -> Int)){
      expr match{
        case v:And        => 
        case v:Or         =>
        case v:Plus       =>
        case v:Minus      => 
        case v:Times      => 
        case v:Div        => 
        case v:LessThan   => 
        case v:Equals     => 
        case v:MethodCall =>  
        case v:IntLit     => Ldc(v.value)
        case v:StringLit  => Ldc(v.vlaue)
        case v:True       => ICONST_1
        case v:False      => ICONST_0
        case v:Identifier => 
        case v:This       => 
        case v:Null       => ACONST_NULL
        case v:New        => DefaultNew("L"+v.tpe.value+";")
        case v:Not        => INEG 
        case v:Block      => (v.exprs map  generateExpr(ch, _, slotFor)) foldLeft (Nil) (_:::_)
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
