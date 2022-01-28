package brainfuck

import brainfuck.Command

/** the brainfuck lang virtual machine which simulates the design model and the behaviour ot the base of the lang */
class BrainFuckVirtualMachine {

  private var byteCells: Array[Byte] = Array.fill[Byte](1)(0)
  private var dataPointer: Int = 0

  /** increment the data pointer by one */
  def increaseDataPointer() = {
    if(dataPointer+1 == byteCells.length) {
      byteCells = byteCells :+ 0
    }
    dataPointer += 1
  }

  /** decrease the data pointer by one */
  def decreaseDataPointer() = if(dataPointer > 0) dataPointer -= 1

  /** increase the value of the cell pointed by the data pointer by one */ 
  def increseCellValue() = byteCells(dataPointer) = (byteCells(dataPointer).toInt + 1).toByte

  /** decrease the value of the cell pointed by the data pointer by one */ 
  def decreaseCellValue() = byteCells(dataPointer) = (byteCells(dataPointer).toInt - 1).toByte

  /** return the value of the cell pointed by the data pointer */ 
  def outputCellValue(): Byte = byteCells(dataPointer)

  /** sets the value of the cell pointed by the data pointer with the one given */ 
  def inputCellValue(x: Byte) = byteCells(dataPointer) = x
  
}