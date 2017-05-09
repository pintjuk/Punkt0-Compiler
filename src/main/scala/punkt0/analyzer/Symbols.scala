package punkt0
package analyzer

import Types._
import scala.collection.immutable.ListMap
object Symbols {

  trait Scope {
     def lookupVarNotArg(n: String): Option[VariableSymbol]
     def lookupVar(n: String, err:Boolean=false, pos: Positioned = NoPosition): Option[VariableSymbol] 
  }

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

  class GlobalScope extends Symbol with Scope{
    val name = "globalScope"
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()
    
    def lookupVarNotArg(n:String): Option[VariableSymbol] = None
    def lookupVar(n: String, err:Boolean=false, pos: Positioned = NoPosition): Option[VariableSymbol] = None 



    def lookupClass(n: String, err:Boolean = false, pos: Positioned = NoPosition): Option[ClassSymbol] =  classes get n match{
      case None =>  if(err) Reporter.error(" class " + n + " is undefined", pos); 
                    None;
      case Some(sym) => Some(sym);
    }
  }

  class ClassSymbol(val name: String) extends Symbol with Scope{
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()
    

    def lookupVarNotArg(n: String): Option[VariableSymbol] = lookupVar(n)

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
  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol with Scope{
    var params:Map[String, VariableSymbol] = ListMap[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var retType: Type= null
    var overridden: Option[MethodSymbol] = None

    def isLocalVar(n: String): Boolean = {
      members get n match {
        case None    => false;
        case Some(x) => true;
      }
    }
    def isArg(n: String): Boolean = {
      params get n match {
        case None    => false;
        case Some(x) => true;
      }
    }
    def isField(n: String): Boolean = {
      classSymbol.lookupVar(n) match {
        case None    => false;
        case Some(x) => true;
      }
    }

    def lookupVarNotArg(n: String): Option[VariableSymbol] = members get n match{
      case None => classSymbol.lookupVarNotArg(n)
      case v    => v
    }


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
