package exam

import exam.LogParser
import scala.io.Source

/** Application */
object Main extends App {
  // reads one by one the arguments provided from the command line
  args.foreach(arg => {
    // grabs the content of the file
    val src = Source.fromFile(arg)
    val content = src.mkString
    src.close()
    // parses the content of the file using the LogParser
    val parser = new LogParser()
    parser.parseAll(parser.tasks, content) match {
      case parser.Success(tasks, _) => 
        tasks.foreach(task => {
          Console.println(task._1)
          task._2.zip(0 until task._2.size).foreach(result => Console.println("[op" + result._2 + "] " + result._1))
        })
      case x => Console.println(x.toString())
    } 
  })
}