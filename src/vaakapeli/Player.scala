package vaakapeli
import scala.math._
import java.awt.Color

class Player(val name: String, val color: Color) {
  
  var points = 0
  
  def getPoints() = points
  
  override def toString() = this.name
  
}