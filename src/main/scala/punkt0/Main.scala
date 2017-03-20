package punkt0

import java.io.File

import lexer._
import ast._


object Main {



  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)
       
      case "--print" :: args =>
      	ctx = ctx.copy(doPrintMain = true)
      	processOption(args)
      	
      case "--ast" :: args =>
      	ctx = ctx.copy(doAST = true)
      	processOption(args) 
        
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)

      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }
	
    ctx
  }

  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" -d <outdir>   generates class files in the specified directory")
    println(" --tokens 		displays tokes")
    println(" --print		prints the parsed program")
    println(" --ast			prints out the AST")
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)
   // println(ctx);
    
    //move later?
    if(ctx.file == None){
      println("no file");
      sys.exit(1);
    }
    
          // case class If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree])
    // val str = Printer.apply(new Trees.If(new Trees.Not(new Trees.IntLit(12)), new Trees.Assign(new Trees.Identifier("value"),new Trees.Plus(new Trees.IntLit(3), new Trees.IntLit(2))), null));
    // println(str);
    // val program = Parser.run(tokenIter)(ctx);
    
    
    if(ctx.doTokens){
      val tokenIter = Lexer.run(ctx.file.get)(ctx);
      var break=false;
      while(!break){
        val t = tokenIter.next;
        val str = t.posString;
        //print(str);
        //println("\t" + t);
        println(t+"("+t.line+":"+t.column+")");
        if(t.kind==EOF) break=true;
      }
      Reporter.terminateIfErrors();
      return 
    }else{
      val tree = Lexer.andThen(Parser).run(ctx.file.get)(ctx);
      if(ctx.doAST){
        println(tree);
      }
      if(ctx.doPrintMain){
        println(Printer.apply(tree));
      }
    }
  }

}
