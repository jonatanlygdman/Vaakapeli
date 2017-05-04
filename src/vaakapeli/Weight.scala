package vaakapeli

class Weight(var owner: Player) extends StackableObject {
  
  def changeOwner(newOwner: Player) = {
    owner = newOwner
  }
  
  def totalWeight: Int = 1
  
  override def toString() = "Weight owned by " + owner.name
  
}