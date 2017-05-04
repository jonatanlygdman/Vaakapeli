package vaakapeli

import scala.collection.mutable.Buffer
import scala.math._


class Scale(val size: Int) extends StackableObject {
   
  //keeps track of all the weights on a scale
  val weights = Array.ofDim[StackableObject](size).map(x => Buffer[StackableObject]())
  
  def totalWeight: Int = {
    var sum = 0
    for (buffer <- weights; scaleOrWeight <- buffer) {
      sum += scaleOrWeight.totalWeight
      }
    sum
  }
  
  //does nothing, implemented to make the StackableObject class work better
  def changeOwner(newOwner: Player) = {}
  
  def countPoints(player: Player): Int = {
    var sum = 0
    for(i <- weights.indices){
      if(!weights(i).isEmpty) {
        if(weights(i)(0).isInstanceOf[Weight]){
          if(weights(i)(0).asInstanceOf[Weight].owner == player){
          sum += weights(i).size * abs(i - size / 2)
        }
      } else {
        sum += weights(i)(0).asInstanceOf[Scale].countPoints(player) * abs(i - size / 2)
      }
    }
  }
    sum
  }
  
  //adds a weight in the desired spot. Coordinates represent spot on scale from left to right
  def addWeight(addOn: Weight, coordinates: Int, adder: Player): Boolean = {
      
    var success = false
    
    this.weights(coordinates) += addOn
    
      if(this.isBalanced){
        //changes all the weights in the location to be owned by this player
      this.weights(coordinates).map(_.changeOwner(adder))
      success = true
      } else {
      removeWeight(addOn, coordinates)
      }
    
    success
    }
  
  //Removes a weight
  def removeWeight(addOn: Weight, coordinates: Int) = {
    this.weights(coordinates) -= addOn
  }
  
  //adds a scale onto the existing one and "closes" the buffer, so nothing can be added to it anymore
  def addScale(addOn: Scale, coordinates: Int) = {
    weights(coordinates) += addOn
    weights(coordinates).toVector
  }
  
  //checks the balance of the scale
  def isBalanced: Boolean = {
    var balance = true
    var rightWeight = 0
    var leftWeight = 0
    var spot = 0
    
    //first split the scale into the left and right parts
    val parts = weights.splitAt(this.weights.size / 2)
    val firstUsable = parts._1
    val secondUsable = parts._2.tail
    
    //then examine the parts by adding their weight to leftWeight, in either case if it's a weight or another scale we encounter
    for(i <- firstUsable){
      if(!i.isEmpty){
      if(i(0).isInstanceOf[Weight]){
      leftWeight += i.size * abs(spot - this.weights.size / 2)
      } else {
        if(!i(0).asInstanceOf[Scale].isBalanced) balance = false
        leftWeight += i(0).totalWeight * abs(spot - this.weights.size / 2)
        }
      }
      spot += 1
    }
    
    //and the same thing for the right part
    for(i <- secondUsable){
      if(!i.isEmpty){
      if(i(0).isInstanceOf[Weight]){
      rightWeight += i.size * abs(spot + 1 - this.weights.size / 2)
      } else {
        if(!i(0).asInstanceOf[Scale].isBalanced) balance = false
        rightWeight += i(0).totalWeight * abs(spot + 1 - this.weights.size / 2)
        }
      }
      spot += 1
    }
    
    //compare left and right after their weights have been calculated
    if(!(abs(rightWeight - leftWeight) <= this.weights.size / 2)) balance = false
    balance
  }
  
  override def toString() = "Scale with the weight " + this.totalWeight
  
}