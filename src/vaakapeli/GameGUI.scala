package vaakapeli

import scala.swing._
import javax.swing.UIManager
import scala.swing.event.MouseClicked
import javax.swing.ImageIcon
import scala.util.Random
import scala.collection.mutable.Buffer


object GameGUI extends SimpleSwingApplication {
  
  
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  
  //The size of the game board is easily changed by changing the value of this. A 40 by 40 game grid is a nice size for the screen 
  //of my pc, but this can be changed to the user's preference.
  val gridWidth = 40
  
  //Buffer of players in the game
  val players = Buffer[Player]()
  
  //Array of colors that will be assigned to the players
  val colors = Array[Color](new Color(0, 255, 255), new Color(255, 255, 0), new Color(0, 255, 0), new Color(255, 0, 0))
  
  //function that creates the players
  createPlayers(0)
  
  //The game itself
  lazy val game = new Game(players.toArray, 10)
  
  //Returns a pair of arrays, the first part wich contains the vertical element of a scale and the second part the horizontal.
  //Return._2 value depends on the size of the parameter scale, ._1 is always of size two.
  def indicesForScale(start: Int, scale: Scale): (Array[Int], Array[Int]) = {
    scale.size match {
      case 3 => (Array[Int](start, start - gridWidth), Array[Int](start - gridWidth * 2 - 1, start - gridWidth * 2, start - gridWidth * 2 + 1))
      
      case 5 => (Array[Int](start, start - gridWidth), Array[Int](start - gridWidth * 2 - 2, start - gridWidth * 2 - 1, 
          start - gridWidth * 2, start - gridWidth * 2 + 1, start - gridWidth * 2 + 2))
      
      case 7 => (Array[Int](start, start - gridWidth), Array[Int](start - gridWidth * 2 - 3, start - gridWidth * 2 - 2, 
          start - gridWidth * 2 - 1, start - gridWidth * 2, start - gridWidth * 2 + 1, start - gridWidth * 2 + 2, start - gridWidth * 2 + 3))
      
      case 9 => (Array[Int](start, start - gridWidth), Array[Int](start - gridWidth * 2 - 4, start - gridWidth * 2 - 3, 
          start - gridWidth * 2 - 2, start - gridWidth * 2 - 1, start - gridWidth * 2, start - gridWidth * 2 + 1, 
          start - gridWidth * 2 + 2, start - gridWidth * 2 + 3, start - gridWidth * 2 + 4))
    }
  }
  
  //Sets all the squares that are playable in the game. In other words all the spots that are on top of a scale, but not in the middle of it.
  def setPlayable(scale: Array[Int]): Unit = {
    for(i <- scale){
      if(i - scale.head != (scale.last - scale.head) / 2 && i - gridWidth >= 0){
      isPlayable(i - gridWidth) = (true, i - gridWidth)
      }
    }
  }
  
  //Keeps track of the weights and scales assigned to the game board
  val weightArray = Array.ofDim[StackableObject](gridWidth * gridWidth)
  
  //Counts the weight of each element on the game board
  val countingArray = Array.ofDim[Int](gridWidth * gridWidth)
  
  //Array that keeps track of all the playable spots on the board
  val isPlayable = Array.ofDim[Boolean](gridWidth * gridWidth).zipWithIndex
  
  //create grid that the game takes place in
  val grid = new GridPanel(gridWidth, gridWidth) {
    this.background = new Color(255, 255, 255)
    contents ++= (1 to gridWidth * gridWidth).map(_ => new Label(""){
      this.background = new Color(255, 255, 255)
      })
    this.vGap = 2
    this.hGap = 2
    
    //Assign weight to index in the weightArray 
    def assignPlayer(weight: Weight, x: Int, y: Int) = {
      val indexCalculator = ((x / 26).toInt + (gridWidth * (y / 22).toInt)).toInt
      weightArray(indexCalculator) = weight
    }

    //This method is responsible for drawing the picture of the scale onto the game board.
    def drawScale(scale: Scale, horizontalComponent: Array[Int], verticalComponent: Array[Int]) = {

        for(i <- verticalComponent) {
          weightArray(i) = scale
          this.contents(i) = new Label {
            icon = new ImageIcon("rsz_pystyviiva.png")
          }
         }
         
         for(i <- horizontalComponent){
           weightArray(i) = scale
           if(i == horizontalComponent.head){
             this.contents(i) = new Label {
            icon = new ImageIcon("leftCorner.png")
          }
           } else if(i == horizontalComponent.last) {
             this.contents(i) = new Label {
            icon = new ImageIcon("Rightcorner.png")
          }
           } else {
           this.contents(i) = new Label {
            icon = new ImageIcon("rsz_vaakaviiva.png")
          }
        }
      }
    }
    
    //By then calling it, the initial scale defined in the game class is drawn. 
    //It draws the foot of the scale to the middle of the bottom row.
    drawScale(game.initialScale, indicesForScale(gridWidth * gridWidth - gridWidth / 2, game.initialScale)._2,
        indicesForScale(gridWidth * gridWidth - gridWidth / 2, game.initialScale)._1)
         
    setPlayable(indicesForScale(gridWidth * gridWidth - gridWidth / 2, game.initialScale)._2)
    
    this.listenTo(this.mouse.clicks)
    
    //Set reaction to mouse click, essentially everything that happens during a turn takes place here

    this.reactions += {
      case MouseClicked(_, point, _, _, _) => 
        
        val getIndex = ((point.x / 26).toInt + (gridWidth * (point.y / 22).toInt)).toInt
        
        //if the game has not ended and the spot that was clicked is playable
        if(!game.hasEnded){
        if(isPlayable(getIndex)._1) {
        
        //some vals to help calculate all the indices the current scale covers
        val foot = weightArray.zipWithIndex.filter(_._1 == weightArray(getIndex + gridWidth)).maxBy(_._2)
        val cord = indicesForScale(foot._2, weightArray(getIndex + gridWidth).asInstanceOf[Scale])._2(0)
        
        //create a new weight that's owner is the player in turn
        val weight = new Weight(game.getTurn())
        
        //adds the weight in the code itself. This is a bit strange in a way because the addWeight method changes the state of the game,
        //and I think it's not the most beatiful of solutions to change state of the game in a if-statement, 
        //but it checks if it's okay to place a weight here, and returns a boolean indicating if it succeeded or not.
        if(weightArray(getIndex + gridWidth).asInstanceOf[Scale].addWeight(weight, getIndex + gridWidth - cord, weight.owner) 
            && game.initialScale.isBalanced) {
          
        //add the weight in the GUI
        assignPlayer(weight, point.x, point.y)
        
        //Add the displayable number value
        countingArray(getIndex) += 1
        
        //Create a cell with the number of weights in that spot.
        contents(getIndex) = new Label(countingArray(getIndex).toString()){
          opaque = true

      //Assign a background color to the cell, indicating the owner of the weight stack.
      //Apparently you can't pattern match with an array, or I wasn't successful at least, so I did it like this.
          if(weight.owner == players(0)){
            this.background = new Color(0, 255, 255)
          } else if(weight.owner == players(1)){
            this.background = new Color(255, 255, 0)
          } else if(weight.owner == players(2)){
            this.background = new Color(0, 255, 0)
          } else if(weight.owner == players(3)){
            this.background = new Color(255, 0, 0)
          }
            
      }
    } else {
      //If move is not successful remove the weight and alert the player about it.
      weightArray(getIndex + gridWidth).asInstanceOf[Scale].removeWeight(weight, getIndex + gridWidth - cord)
      Dialog.showMessage(box, "You can't place a weight there!", "Illegal move")
    }
        
        //change turn
        game.changeTurn()
     
        turns.text = "Your turn " + game.getTurn() + "!   Turns remaining: " + game.turnsRemaining.toString()
        
        //repaint the view on the screen
        revalidate()
      }
        
    //Create a new scale on the game board every time it's the first player's turn, except on the first turn.
         if(game.turnIndex % 5 == 0 && game.turnIndex > 0) {
         
         var scaleIsSet = false
         var stopEndlessLoop = 0

         //tries creating a scale in a vacant spot until it succeeds or loops 1000 times.
         while(scaleIsSet == false && stopEndlessLoop < 1000){  

         val scale = game.createScale()
         val possibleSpots = isPlayable.filter(_._1).map(_._2)
         val randomSpot = possibleSpots(Random.nextInt(possibleSpots.size))
         val horizontalComponent = indicesForScale(randomSpot, scale)._2
         
         //if the scale we are trying to create doesn't overlap with any already existing scale or go out of bounds...
         if(weightArray.zipWithIndex.filter(_._1 != null).map(x => x._2).filter(
             (horizontalComponent ++ indicesForScale(randomSpot, scale)._1).contains).forall(_ == null)
            && horizontalComponent.zip(horizontalComponent.tail).dropWhile(x => x._1 / gridWidth == x._2 / gridWidth).isEmpty) {
         
         //...we can create it
         setPlayable(horizontalComponent)
         
         //again some val:s to help with calculating indices for the scale
         val foot = weightArray.zipWithIndex.filter(_._1 == weightArray(randomSpot + gridWidth)).maxBy(_._2)
         val cord = indicesForScale(foot._2, weightArray(randomSpot + gridWidth).asInstanceOf[Scale])._2(0)
         
         weightArray(randomSpot + gridWidth).asInstanceOf[Scale].addScale(scale, randomSpot + gridWidth - cord)
         
         //set the images for the spots the scale covers
         drawScale(scale, horizontalComponent, indicesForScale(randomSpot, scale)._1)
         
         //and the scale is set and we can stop the loop
         scaleIsSet = true
        }
         stopEndlessLoop += 1
      }
    }
       //Count the points for each player
       for(player <- players){
         player.points = game.initialScale.countPoints(player)
       }
  
       //Update the text in the labels
        var i = 0
        for(label <- scores.contents){
          label.asInstanceOf[Label].text = "  Player " + (i + 1) + ": " + players(i).toString() + " with " + players(i).getPoints() + " points  "
          i += 1
          }
       } else {
         //when the game ends show this
         Dialog.showMessage(box, "Congratulations " + game.winner + ", you have won the game with a score of " + game.winner.points + "!", "Game Over")
       }
    }
  }
  
  //create window and frame for the game to take place in
  
  val window: MainFrame = new MainFrame {
    
    this.title = "Vaakapeli"
    this.resizable = false
    this.menuBar = new MenuBar {
      contents += new Menu("Peli") {
        contents += new MenuItem(Action("Uusi Peli")     { grid.peer.removeAll() } )
        contents += new MenuItem(Action("Apua!")     { Dialog.showMessage(box, "Hello, and welcome to the scale game!\n" +
        "\nIn this game, the purpose is to place weights on top of the scales drawn in the game window. \nYou can do this by " +
        "simply clicking the mouse in the desired location for the weight. \nEach player plays on his own turn, indicated on the " +
        "bottom of the screen. \nThe points are accumulated the more scales lie underneath the one you place your weight on, " +
        "so place your weights wisely! \nYou can also put your weight on top of others', in which case you will steal their points!" +
        "\nBe careful however, if you cause any of the scales to tip over you will lose your turn." +
        "\n \nGood luck and have fun!\n", 
        "Help is here!") } )
        contents += new MenuItem(Action("Lopeta")     { dispose() } )
      }
    }
  }
    
  
  def createPlayers(index: Int): Unit = {
    val r = Dialog.showInput(box, "Insert player name with a maximum of 10 characters." + 
        "\nIf you want to start the game, just leave the field empty and press \"OK\"", 
        "Please enter name for Player " + (index + 1), initial = "")
    r match {
      case Some("") => //An empty string takes us to the game
      case Some(s) =>  //A string with a string registers a player with this name and calls the method again for more players
        if(s.length() < 11){
        players += new Player(s, colors(index))
        if(index < 3){
        createPlayers(index + 1)
          }
        } else {
          createPlayers(index)
        }
      case None => 
    }
  }
  
  //Panel for displaying scores
  val scores = new BoxPanel(Orientation.Horizontal){
    for(i <- players.indices){
    contents += new Label {
     this.text = "  Player " + (i + 1) + ": " + players(i).toString() + " with " + players(i).getPoints() + " points  "
     this.opaque = true
     this.background = players(i).color
      }
    }
  }
  
  val turns = new Label("Your turn " + game.getTurn() + "!   Turns remaining: " + game.turnsRemaining.toString())
  
    val box: BoxPanel = new BoxPanel(Orientation.Vertical){
    contents += grid
    contents += scores
    contents += turns
    this.background = new Color(255, 255, 255)
  }
  
  window.contents = box
  
  def top = window
}
