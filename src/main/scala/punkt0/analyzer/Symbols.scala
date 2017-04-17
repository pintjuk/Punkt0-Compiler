package punkt0
package analyzer

import Types._

object Symbols {

  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }

    def symString: String = _sym match {
      case Some(s) => s.id.toString;
      case None => "??";
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String, err:Boolean = false, pos: Positioned = NoPosition): Option[ClassSymbol] =  classes get n match{
      case None =>  if(err) Reporter.error(" class " + n + " is undefined", pos); 
                    None;
      case Some(sym) => Some(sym);
    }
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String, err:Boolean=false, pos: Positioned = NoPosition): Option[MethodSymbol] = methods get n match {
      case None => parent match { 
        case Some(x) => x.lookupMethod(n, err, pos)
        case None => if(err) Reporter.error(" method " +n+" is undefined", pos);
                     None
      }
      case x    => x
    }
    def lookupVar(n: String, err:Boolean=false, pos: Positioned = NoPosition): Option[VariableSymbol] = members get n match {
      case None => parent match { 
        case Some(x) => x.lookupVar(n, err,pos)
        case None =>  if(err) Reporter.error(" variable " + n + " is undefined", pos);
                      None
      }
      case x    => x
    }
  }
  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(n: String, err:Boolean=false, pos: Positioned = NoPosition): Option[VariableSymbol] = members get n match {
      case None => params get n match {
        case None => classSymbol.lookupVar(n, err, pos);
        case Some(x) => Some(x);
      }
      case x    => x
    }
  }

  class VariableSymbol(val name: String) extends Symbol

}
