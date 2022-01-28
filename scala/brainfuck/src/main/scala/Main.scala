package brainfuck

//import brainfuck.BrainFuckParser
import scala.io.Source
import brainfuck.BrainFuckInterpreter

/** Application */
object Main extends App {
  args.foreach(arg => {
    // reads the content of the file (which should be a brainfuck sript)
    val src = Source.fromFile(arg)
    val content = src.mkString
    src.close()
    // parse and runs the script
    val p = new BrainFuckParser()
    val i = new BrainFuckInterpreter()
    p.parseAll(p.program, content) match {
      case p.Success(x, _) => i.run(x)
      case x => Console.println(x.toString())
    }
  })
}