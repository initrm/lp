package exam

import exam.LogParser
import scala.io.Source

object Main extends App {
  // leggo gli argomenti forniti uno alla volta
  args.foreach(arg => {
    val src = Source.fromFile(arg)
    val content = src.mkString
    src.close()
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