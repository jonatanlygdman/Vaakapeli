package vaakapeli

abstract class StackableObject {
  
  def changeOwner(newOwner: Player): Unit
  
  def totalWeight: Int
  
}