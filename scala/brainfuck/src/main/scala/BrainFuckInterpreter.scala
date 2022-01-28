package brainfuck

import brainfuck.Command
import brainfuck.BrainFuckVirtualMachine

/** the brainfuck language executor */
class BrainFuckInterpreter {
  def run(commands: List[Command]) = {
    val vm = new BrainFuckVirtualMachine()
    for(cmd <- commands) cmd.execute(vm)
  }
}