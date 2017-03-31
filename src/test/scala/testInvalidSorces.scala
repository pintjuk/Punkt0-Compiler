import collection.mutable.Stack
import org.scalatest._
import java.io.File
import java.io._
import punkt0._
import ast._
import lexer._


class rutTestPrograms extends FlatSpec {
  Reporter.testing=true;
  
  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }


 for(filename <- getListOfFiles("testprograms/lab2/invalid")){
    filename.toString should "throw  RuntimeException when lexed " in{
      var ctx = Context()
      ctx = ctx.copy(doAST = true)
      ctx = ctx.copy(file = Some( filename))
      assertThrows[RuntimeException] {
        Main.printTokens(ctx);
      }
      Reporter.reset;
    }
 }

  for(filename <- getListOfFiles("testprograms/lab3/invalid")){
    filename.toString should "throw  RuntimeException when parsed " in{
      Reporter.reset;
      var ctx = Context()
      ctx = ctx.copy(doAST = true)
      ctx = ctx.copy(file = Some( filename))
      
      assertThrows[RuntimeException] {
        val tokenIter = Lexer.andThen(Parser).run(ctx.file.get)(ctx);
    }
  }
 }

  for(filename <- List(
    "testprograms/lab2/valid/crazyids.p0",
    "testprograms/lab2/valid/positions.p0",
    "testprograms/lab2/valid/endofline.p0"
    )){
    Reporter.reset;
    "tockens of "+filename should "should be equal to"+ filename.dropRight(2)+"check" in{
      var ctx = Context().copy(file = Some(new File( filename)))
      val tokens = Main.mkTokens(ctx);
      Reporter.terminateIfErrors
      val tokensCheck = scala.io.Source.fromFile(new File(filename.dropRight(2)+"check")).mkString;
      println(tokens);
      println(tokensCheck);
      assert(tokens.stripLineEnd === tokensCheck.stripLineEnd);
    }

  }
  Reporter.reset;
 for(filename <- List(
    "testprograms/lab3/valid/99bottles.p0",
    "testprograms/lab3/valid/BinarySearch.p0",
    "testprograms/lab3/valid/Calendar.p0",
    "testprograms/lab3/valid/ComplexNumbers.p0",
    "testprograms/lab3/valid/DrawStuff.p0",
    "testprograms/lab3/valid/Factorial.p0",
    "testprograms/lab3/valid/GCD.p0",
    "testprograms/lab3/valid/HeapSort.p0",
    "testprograms/lab3/valid/Life.p0",
    "testprograms/lab3/valid/Multiplicator.p0",
    "testprograms/lab3/valid/NewtonsMethod.p0",
    "testprograms/lab3/valid/OptimalChange.p0",
    "testprograms/lab3/valid/Polymorphism.p0",
    "testprograms/lab3/valid/PrimeTest.p0",
    "testprograms/lab3/valid/QuickSort.p0",
    "testprograms/lab3/valid/ScalarProduct.p0",
    "testprograms/lab3/valid/Simple.p0",
    "testprograms/lab3/valid/Sudoku.p0",
    "testprograms/lab3/valid/VehicleRent.p0"
  )){
   
  
    Reporter.reset;
    "ast of "+filename should "should be equal "+ filename+".ast" in{
      var ctx = Context()
      ctx = ctx.copy(doAST = true)
      ctx = ctx.copy(file = Some(new File( filename)))
      val tree = Lexer.andThen(Parser).run(ctx.file.get)(ctx);
      val correctAst = scala.io.Source.fromFile(new File(filename+".ast")).mkString;
      val ourAst = tree.toString;
      println(ourAst);
      println(correctAst);
      assert(ourAst.replaceAll("\\s+","") === correctAst.replaceAll("\\s+",""));
    }


    Reporter.reset;

    "ast of prity print of "+filename should "equal its ast" in{
      var ctx = Context()
      ctx = ctx.copy(doAST = true)
      ctx = ctx.copy(file = Some(new File( filename)))
      val tree = Lexer.andThen(Parser).run(ctx.file.get)(ctx);
      val correctAst = scala.io.Source.fromFile(new File(filename+".ast")).mkString;
      val ourAst = tree.toString;


      val tempfilename="/tmp/prittyoutput";
      val tempfile = new FileWriter(new File(tempfilename), false); 
      tempfile.write( Printer.apply(tree)+"\n");
      tempfile.close();

      ctx = ctx.copy(file = Some(new File(tempfilename)))
      val newtree =  Lexer.andThen(Parser).run(ctx.file.get)(ctx);

      //println(tree)
      //println(newtree)
      assert(newtree.toString===tree.toString);
    }

  /*  "pp of ast of pp of "+filename should "equal to its pp its ast" in{
      var ctx = Context()
      ctx = ctx.copy(doAST = true)
      ctx = ctx.copy(file = Some(new File( filename)))
      val tree = Lexer.andThen(Parser).run(ctx.file.get)(ctx);
      val correctAst = scala.io.Source.fromFile(new File(filename+".ast")).mkString;
      val ourAst = tree.toString;


      val tempfilename="/tmp/prittyoutput";
      val tempfile = new FileWriter(new File(tempfilename), false); 
      tempfile.write( Printer.apply(tree)+"\n");
      tempfile.close();

      ctx = ctx.copy(file = Some(new File(tempfilename)))
      val newtree =  Lexer.andThen(Parser).run(ctx.file.get)(ctx);
       assert(Printer.apply(newtree) === Printer.apply(tree) );
    }*/
  }
}
