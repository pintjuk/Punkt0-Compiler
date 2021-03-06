package punkt0

import java.io.File

import lexer._
import ast._
import analyzer._
import code._
import optimize._

object Main {



  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      
      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)
      case "-o" :: args =>
        ctx = ctx.copy(doTailRec = true)
        processOption(args)
      case "--symid" :: args =>
        ctx = ctx.copy(doSymbolIds = true)
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
    println(" --symid   in combination with --print: prints all identifiers symbols")
    println(" --ast			prints out the AST")
    println(" -o			optimize")
  }

  def mkTokens(ctx:Context):String = {
    val tokenIter = Lexer.run(ctx.file.get)(ctx);
    var res="";
    while(tokenIter.hasNext){
      val t = tokenIter.next;
      val str = t.posString;
      res += t+"("+t.line+":"+t.column+")"+"\n";
    }
    return res;
  }

  def printTokens(ctx:Context):Unit = {
    print(mkTokens(ctx))
    Reporter.terminateIfErrors();
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)
    
    //move later?
    if(ctx.file == None){
      println("no file");
      sys.exit(1);
    }
    if(ctx.doSymbolIds){
      var tree = Lexer.andThen(Parser).andThen(NameAnalysis).andThen(TypeChecking).run(ctx.file.get)(ctx);
      if (ctx.doTailRec){
        Reporter.terminateIfErrors();
        Printer.doIds=true;
        println("Before tail recursion elimination")
        println("=================================")
        println(Printer.apply(tree));
        println()
        println("After tail recursion elimination")
        println("================================")
        tree = TailRecElimination.run(tree)(ctx);
      }
      Reporter.terminateIfErrors();
      Printer.doIds=true;
      println(Printer.apply(tree));
    }
    else if(ctx.doTokens){
      printTokens(ctx);
      return
    }else{
      val tree = Lexer.andThen(Parser).run(ctx.file.get)(ctx);
      if(ctx.doAST){
        println(tree);
      }else if(ctx.doPrintMain){
        println(Printer.apply(tree));
      }else{
          var pipe = NameAnalysis .andThen(TypeChecking)
          if(ctx.doTailRec){
            pipe= pipe.andThen(TailRecElimination)
          }
          pipe.andThen(CodeGeneration).run(tree)(ctx);
      }
    }
  }

}
