package vaakapeli

import scala.util.Random
import scala.math._

class Game(val players: Array[Player], var turnsRemaining: Int) {
  
  val possibleSizes = Array(3, 5, 7, 9)
  val initialScale = new Scale(9)
  
  var turnIndex = 0
  
  //Creates a new scale of random size, choosing from possibleSizes
  def createScale(): Scale = {
    new Scale(possibleSizes(Random.nextInt(possibleSizes.size)))
  }
  
  //returns the player in turn
  def getTurn(): Player = players(turnIndex % players.size)
  
  def changeTurn(): Unit = {
    turnIndex += 1
    if(turnIndex % players.size == 0) turnsRemaining -= 1
  }
  
  def hasEnded: Boolean = {
    turnsRemaining <= 0
  }
  
  //the winner is the player with the most points when the game ends
  //this is calculated when the game actually ends
  lazy val winner = players.sortBy(_.points).head
  
}