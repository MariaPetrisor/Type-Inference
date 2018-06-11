package inference

sealed trait Expr

case class Lambda(args: List[Val], body: Expr) extends Expr
case class Val(name: String) extends Expr
case class Apply(fun: Expr, args: List[Expr]) extends Expr
case class Const(int: Integer) extends Expr

trait BinaryOperation extends Expr{
  def expr1: Expr
  def expr2: Expr
}

case class Add(expr1: Expr, expr2: Expr) extends BinaryOperation
case class Sub(expr1: Expr, expr2: Expr) extends BinaryOperation
case class Mul(expr1: Expr, expr2: Expr) extends BinaryOperation
case class Diff(expr1: Expr, expr2: Expr) extends BinaryOperation

case class Eq(expr1: Expr, expr2: Expr) extends Expr
case class If(cond: Expr, expr1: Expr, expr2: Expr) extends Expr
case class ValDecl(var_value: Map[Val, Expr], body: Expr) extends Expr



trait Type{
  def x: String;
}

case class IntType(x: String) extends Type
case class FuncType(x: String) extends Type

object TypeChecker extends App{
  def infer(expr: Expr, env: Map[Val, Expr]): Type = expr match{
    case Const(_) => IntType("a")
    case Val(_) =>
      IntType("a")
    case Add(lhs, rhs) =>
      val typeL = infer(lhs, env)
      val typeR = infer(rhs, env)
      (typeL, typeR) match {
        case (IntType(_),IntType(_)) => IntType("a")
        case (_, _) => throw binOpException("The types of both expressions have to be of the Integer types")
      }
    case Mul(lhs, rhs) =>
      val typeL = infer(lhs, env)
      val typeR = infer(rhs, env)
      (typeL, typeR) match {
        case (IntType(_),IntType(_)) => IntType("a")
        case (_, _) => throw binOpException("The types of both expressions have to be of the Integer types")
      }
    case Sub(lhs, rhs) =>
      val typeL = infer(lhs, env)
      val typeR = infer(rhs, env)
      (typeL, typeR) match {
        case (IntType(_),IntType(_)) => IntType("a")
        case (_, _) => throw binOpException("The types of both expressions have to be of the Integer types")
      }
    case Diff(lhs, rhs) =>
      val typeL = infer(lhs, env)
      val typeR = infer(rhs, env)
      (typeL, typeR) match {
        case (IntType(_),IntType(_)) => IntType("a")
        case (_, _) => throw binOpException("The types of both expressions have to be of the Integer types")
      }
    case Eq(lhs, rhs) =>
      val typeL = infer(lhs, env)
      val typeR = infer(rhs, env)
      (typeL, typeR) match {
        case (IntType(_),IntType(_)) => IntType("a")
        case (_, _) => throw binOpException("The types of both expressions have to be of the Integer types")
      }
    case If(cond, branch1, branch2) => infer(cond, env) match {
      case IntType(_) =>
        val typeB1 = infer(branch1, env)
        val typeB2 = infer(branch2, env)
        (typeB1, typeB2) match {
          case (IntType(_),IntType(_)) => IntType("a")
          case (FuncType(_), FuncType(_)) => FuncType("a")
          case (_, _) => throw ifBranchesException("Both branches return something different")
      case _ => throw ifBranchesException("If condition does not evaluate to IntType")
      }
    }
    case ValDecl(var_value, body) =>
      if(var_value.isEmpty) {
        throw emptyListException("Variable list in val-decl should not be empty")
      }
      else {
        infer(body, env ++ var_value)
      }

    case Lambda(args, body) =>
      val (new_env, remainingArgs) = updateEnv(env, args)
        addToString(remainingArgs, infer(body, new_env))

    case Apply(fun, expr_list) =>
      infer(fun, addToEnv(env, expr_list))
  }

  def addToString(valList: List[Val], _type: Type): Type = valList match {
    case Nil => _type
    case _ :: tail => addToString(tail, FuncType("a -> " + _type.x))
  }

  //Adds the values assigned by apply to env using strings with numbers (before knowing the Val names from Lambda)
  def addToEnv(env: Map[Val, Expr], valList: List[Expr]): Map[Val, Expr] = valList match {
    case Nil => env
    case head :: tail => addToEnv(env + (Val(getValidNumber(env, 0)) -> head), tail)
  }

  def isAllDigits(x: String) = x forall Character.isDigit

  def getApplyVal(env: Map[Val, Expr], min: Int): Val = {
    if (min > 1000){
      Val("-1")
    }
    else {
      if (env.contains(Val(min.toString))) {
        Val(min.toString)
      }
      else {
        getApplyVal(env, min + 1)
      }
    }
  }

  def getValidNumber(env: Map[Val, Expr], int: Int): String = {
    if (env.contains(Val(int.toString))){
      getValidNumber(env, int+1)
    }
    else{
      int.toString
    }
  }

  //Adds the Vals from lambda to the environment and returns a list of the Vals that have no value assigned in apply
  def updateEnv(env: Map[Val, Expr], valList: List[Val]): (Map[Val, Expr], List[Val]) = valList match {
    case Nil => (env, Nil)
    case head :: tail =>
      if (!getApplyVal(env, 0).name.equals("-1")){
        updateEnv(env + (head -> env(getApplyVal(env, 0))) - getApplyVal(env, 0), tail)
      }
      else{
        //we still have elements in the valList with no value from apply, must return function
        (env, head :: tail)
      }
  }
}
