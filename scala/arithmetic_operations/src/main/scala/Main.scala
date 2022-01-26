package arithmetic_operations

import scala.io.Source
import arithmetic_operations.ArithmeticOperationsParser

/** Application */
object Main extends App {
  // reads one by one the arguments provided from the command line
  args.foreach(arg => {
    // reading the file
    val src = Source.fromFile(arg)
    val content = src.mkString
    src.close()

    // file content parsing
    val parser = new ArithmeticOperationsParser()
    parser.parseAll(parser.expression, content) match {
      case parser.Success(x, _) => if(x) Console.println("L'operazione è corretta") else Console.println("L'operazione non è corretta")
      case x => Console.println(x.toString())
    }
  })
}