package exam 

import scala.util.parsing.combinator._
import java.io.File
import java.io.FileWriter
import scala.io.Source

/** Parser for LogLang files */
class LogParser extends JavaTokenParsers {
  
  /** returns Parser[List[(String, List[Boolean])]] */
  def tasks = rep1(task)

  /** returns Parser[(String, List[Boolean])] */
  def task = taskName ~ taskInstructions ^^ { case taskName ~ results => (taskName, results) }

  /** returns Parser[String] */
  def taskName = "task" ~> ident ^^ { case name => name.toString() } 

  /** returns Parser[List[Boolean]] */
  def taskInstructions = "{" ~> rep1(operation) <~ "}"

  /** returns Parser[String] */
  def givenFileName = stringLiteral ^^ { case fileName => fileName.substring(1, fileName.length-1) }

  /**
   * every operation (delete, merge, backup, rename) returns Parser[Boolean] 
   * the boolean value marks if the operation has been completed successfully or not
   */
  def operation = delete | merge | backup | rename
  def delete = "delete" ~> givenFileName ^^ { case fileName => new File(fileName).delete() }
  def merge = "merge" ~> givenFileName ~ givenFileName ~ givenFileName ^^ {
    case srcOneFileName ~ srcTwoFileName ~ destFileName => {
      try {
        // creates the file with the desired name, if it fails, the operation is marked as failed too
        if(! new File(destFileName).createNewFile()) false
        // scrivo all'interno del file creato il contenuto dei due file che si vogliono unire
        val fw = new FileWriter(destFileName)
        val srcOne = Source.fromFile(srcOneFileName)
        val srcTwo = Source.fromFile(srcTwoFileName)
        fw.write(srcOne.mkString + srcTwo.mkString)
        fw.close()
        srcOne.close()
        srcTwo.close()
        true // operation succeded
      } catch {
        case e: Exception => false // error, operation failed
      }
    }
  }
  def backup = "backup" ~> givenFileName ~ givenFileName ^^ {
    case sourceFileName ~ destFileName =>
      try {
        // creates the file with the desired name, if it fails, the operation is marked as failed too
        if(! new File(destFileName).createNewFile()) false
        // copies the content of the file in the new one
        val fw = new FileWriter(destFileName)
        val content = Source.fromFile(sourceFileName)
        fw.write(content.mkString)
        fw.close()
        content.close()
        true
      } catch {
        case e: Exception => false // error, operation failed
      }
  }
  def rename = "rename" ~> givenFileName ~ givenFileName ^^ { 
    case oldFileName ~ newFileName => new File(oldFileName).renameTo(new File(newFileName))
  }

}