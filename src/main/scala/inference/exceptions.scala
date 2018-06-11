package inference

case class expectedIntegerException(message: String)  extends Exception(message)
case class emptyListException(message: String)  extends Exception(message)

case class unboundVariableException(message: String)  extends Exception(message)
case class binOpException(message: String)  extends Exception(message)
case class ifBranchesException(message: String)  extends Exception(message)

