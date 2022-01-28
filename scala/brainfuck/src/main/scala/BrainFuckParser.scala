package brainfuck

import scala.util.parsing.combinator._
import scala.io.StdIn
import brainfuck.BrainFuckVirtualMachine

/** defining command trait, every class that implements that trait is a valid bf command */
trait Command {
  def execute(vm: BrainFuckVirtualMachine)
}
class IncreaseDataPointer extends Command {
  override def execute(vm: BrainFuckVirtualMachine) = vm.increaseDataPointer()
}
class DecreaseDataPointer extends Command {
  override def execute(vm: BrainFuckVirtualMachine) = vm.decreaseDataPointer()
}
class IncreseCellValue extends Command {
  override def execute(vm: BrainFuckVirtualMachine) = vm.increseCellValue()
}
class DecreaseCellValue extends Command {
  override def execute(vm: BrainFuckVirtualMachine) = vm.decreaseCellValue()
}
class OutputCellValue extends Command {
  override def execute(vm: BrainFuckVirtualMachine) = Console.print(vm.outputCellValue().toChar)
}
class InputCellValue extends Command {
  override def execute(vm: BrainFuckVirtualMachine) = vm.inputCellValue(StdIn.readByte())
}
class Loop(commands: List[Command]) extends Command {
  override def execute(vm: BrainFuckVirtualMachine) = while(vm.outputCellValue().toInt != 0) for(cmd <- commands) cmd.execute(vm)
}

/** parser for the brain fuck lang */
class BrainFuckParser extends JavaTokenParsers {

  def program: Parser[List[Command]] = rep(command)

  def command: Parser[Command] = 
    increaseDataPointer | decreaseDataPointer | increaseCellValue | decreaseCellValue | outputCellValue | inputCellValue | loop

  def increaseDataPointer: Parser[IncreaseDataPointer] = ">" ^^ { case x => new IncreaseDataPointer() }
  def decreaseDataPointer: Parser[DecreaseDataPointer] = "<" ^^ { case x => new DecreaseDataPointer() }
  def increaseCellValue: Parser[IncreseCellValue] = "+" ^^ { case x => new IncreseCellValue() }
  def decreaseCellValue: Parser[DecreaseCellValue] = "-" ^^ { case x => new DecreaseCellValue() }
  def outputCellValue: Parser[OutputCellValue] = "." ^^ { case x => new OutputCellValue() }
  def inputCellValue: Parser[InputCellValue] = "," ^^ { case x => new InputCellValue() }
  def loop: Parser[Loop] = "[" ~> program <~ "]" ^^ { case x => new Loop(x) }

}