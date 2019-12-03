package scala

import scala.collection.immutable.{ArraySeq, Map}
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object isolation_game {
  def main(args:Array[String]): Unit ={

    //initialise the board with 1 and the players starting cells of the players "A" and "B". Players will be places differently if the number of the column is odd or even
    def initBoard (dim:(Int, Int)) : (ArraySeq[ArraySeq[Any]],Map[String,(Int, Int)]) = {
      val x = dim._1
      val y = dim._2

      val board = ArraySeq.tabulate(x, y){
        case (a, b) =>
          if(y%2 == 0) { //if number of column is even player B will be placed at N/2 and player A at (N/2)-1
            if(a == 0 && b == y/2){"B"}
            else if(a == x-1 && b == (y/2)-1){"A"}
            else{1}
          }
          else{ //if number of column is odd players are placed in the central column
            if(a == 0 && b == y/2){"B"}
            else if(a == x-1 && b == y/2){"A"}
            else{1}
          }

      }
      //positions of the players into a map
      if(y%2 == 0) {
        val players = Map("A" -> (x-1,y/2 - 1), "B" -> (0,y/2))
        return (board,players)
      }
      else{
        val players = Map("A" -> (x-1,y/2), "B" -> (0,y/2))
        return (board,players)
      }
    }

    //display the board in a comprehensive way for the players with the numbers on the rows and columns
    def display(board:ArraySeq[ArraySeq[Any]]): Unit ={
      println(".  "+List.range(0, board(0).length).mkString("   "))
      println(".  "+List.fill(board(0).length)("_").mkString("   "))
      //  println(board.map(_.mkString("   ")).mkString("\n"))
      board.zipWithIndex.foreach{ case(row, i) => println(i+" |"+row.mkString("   "));}
    }

    //get an row and column integers numbers within bounds
    def get_xy(correct_inputs:String,bounds:(Int,Int)): (Int,Int) = {
      val pattern = correct_inputs
      //read input of the player
      print("Row : ")
      val x = readLine()
      print("Column : ")
      val y = readLine()

      if(!x.matches(pattern) || !y.matches(pattern)){ //matches correct inputs
        println("Wrong inputs")
        get_xy(correct_inputs,bounds)
      }
      else{
        if(x.toInt >= bounds._1 || y.toInt >= bounds._2){ //within bounds
          println("Out of bounds")
          get_xy(correct_inputs,bounds)
        }
        else {
          return (x.toInt, y.toInt)
        }
      }

    }

    //remove one cell of the board within bounds and available cells (can't remove the players starting cells)
    def remove_cell(board:ArraySeq[ArraySeq[Any]], player:String) : ArraySeq[ArraySeq[Any]]={
      println("Player "+player+" remove turn")
      val (x,y) = get_xy(correct_inputs="[0-9]",bounds=(board.length,board(0).length))
      if(board(0).length%2 == 0) { //board columns is even
        if(board(x)(y) != 1 || (x,y) == (board.length-1,board(0).length/2 - 1) || (x,y) == (0,board(0).length/2)){ //if it's removing an unavailable cell (0) or the players starting cells
          println("Invalid cell selected")
          remove_cell(board,player)
        }
        else{
          return board_update(board,position=(x,y),piece=0) //update position with an unavailable cell (0)
        }
      }
      else if(board(x)(y) != 1 || (x,y) == (board.length-1,(board(0).length-1)/2) || (x,y) == (0,(board(0).length-1)/2) ){ //board columns is odd
          print("Invalid cell selected\n")
          remove_cell(board,player)
        }
        else{
          return board_update(board,position=(x,y),piece=0)
        }
    }

    //get a new board with the piece (either players "A" and "B" or "1" or "0") on the chosen position
    def board_update(board:ArraySeq[ArraySeq[Any]],position:(Int,Int),piece:Any): ArraySeq[ArraySeq[Any]]={
      val row = position._1
      val col = position._2
      val board_update = board.updated(row, board(row).updated(col, piece)) //update the entire column to get the whole new board
      return board_update
    }

    //Move player to a neighboring available cell within board's bounds
    def move(board:ArraySeq[ArraySeq[Any]], players_positions:Map[String,(Int, Int)], player:String): (ArraySeq[ArraySeq[Any]],Map[String,(Int, Int)])={
      print("Player "+player+" move turn\n")

      val (selected_row,selected_col) = get_xy(correct_inputs="[0-9]",bounds=(board.length,board(0).length))

      val player_row = players_positions(player)._1
      val player_col = players_positions(player)._2

      if(board(selected_row)(selected_col) != 1){ //
        print("Invalid cell to move selected : not an available cell\n")
        move(board,players_positions,player)
      }
      else{
        if((player_row-selected_row <= 1 && player_row-selected_row >= -1) && (player_col-selected_col <= 1 && player_col-selected_col >= -1)){ //only neighboring cells
          val board_player_moved = board_update(board,position=(player_row,player_col),piece=1) //player leaves an available cell behind him after he moved
          if(player=="A"){
            val players_updated = Map("A" -> (selected_row,selected_col), "B" -> players_positions("B")) //update position
            val board_updated = board_update(board_player_moved,position=(players_updated(player)._1,players_updated(player)._2),piece=player) //update board with new position
            return (board_updated,players_updated)
          }
          else{//if player == 'B'
            val players_updated = Map("A" -> players_positions("A"), "B" -> (selected_row,selected_col))
            val board_updated = board_update(board_player_moved,position=(players_updated(player)._1,players_updated(player)._2),piece=player)
            return (board_updated,players_updated)
          }
        }
        else{
          print("Invalid cell to move selected : too far\n")
          move(board,players_positions,player)
        }
      }
    }

    //Play each turn until one of the players wins
    def play(board:ArraySeq[ArraySeq[Any]], players: Map[String,(Int, Int)]): Unit = {
      val playerA = "A"
      val playerB = "B"

      //move turn for player A
      check_turn(board,players,playerA,move=true)
      val (board_moveA,players_updateA) = move(board,players,playerA)
      display(board_moveA)

      //remove turn for player A
      check_turn(board_moveA,players_updateA,playerA,move=false)
      val board_removeA = remove_cell(board_moveA,playerA)
      display(board_removeA)

      //move turn for player B
      check_turn(board_removeA,players_updateA,playerB,move=true)
      val (board_moveB,players_updateB) = move(board_removeA,players_updateA,playerB)
      display(board_moveB)

      //remove turn for player B
      check_turn(board_moveB,players_updateB,playerB,move=false)
      val board_removeB = remove_cell(board_moveB,playerB)
      display(board_removeB)

      play(board_removeB,players_updateB) //next turn
    }

    //check if the turn is possible (either possible to move or possible to remove) and if not, the opponent wins
    def check_turn(board: ArraySeq[ArraySeq[Any]], players: Map[String, (Int, Int)], player: String, move: Boolean): Unit = {
      val x = players(player)._1
      val y = players(player)._2
      val opponent = Map("A" -> "B", "B" -> "A")
      val bound_moves = ListBuffer[Any]() //mutable list to add every available moves within bounds (example : left move -> (x-1,y+0) only if x-1 >= 0)
      for(i <- -1 to 1) {
        for(j <- -1 to 1){
          if( (x+i > -1) && (x+i < board.length) && (y+j > -1) && (y+j < board(0).length) && ((i,j) != (0,0)) ){ //No out of bound moves + No staying in place move
            if(board(x+i)(y+j) != opponent(player) && board(x+i)(y+j) != 0){ //can't move on opponent and can't move on removed cell (0)
              if(board(0).length%2 == 0){ //board columns is even
                if(!(!move && ((x+i,y+j) == (board.length-1,board(0).length/2 - 1) || (x+i,y+j) == (0,board(0).length/2)))) { //if it's not removing the players starting cells
                  bound_moves += board(x+i)(y+j) //add this move as an available move
                }
              }
              else if(!(!move && ((x+i,y+j) == (board.length-1,(board(0).length-1)/2) || (x+i,y+j) == (0,(board(0).length-1)/2)))){ //board columns is odd
                  bound_moves += board(x+i)(y+j)
                }
            }
          }
        }
      }

      val moves = bound_moves.toList //mutable list to immutable list

      //winning condition
      if(!moves.contains(1)){ //if there's no available (1) cell
        if(moves.isEmpty){ //if there's no move possible
          print("No allowed move left : ")
        }
        println("Player "+opponent(player)+" won")
        System.exit(0)
      }
    }

    println("Welcome to the Isolation game. You are the player A and your goal is to isolate the player B\n" +
      "You can choose the dimensions of the board up to 10x10 (original game : 7x7).\n" +
      "- Cells with a '1' mean an available cell and '0' mean an unavailable (removed) cell\n" +
      "- A and B are the players\n" +
      "- Each turn you move to a neighboring cell if it's available and then remove any cell of the board except the players and the starting cells of the players\n" +
      "- The first player who can't play its turn loses (either can't move or remove a cell)\n" +
      "Good luck !")
    println("Choose dimensions of the board :")
    val (board,players) = initBoard(get_xy(correct_inputs="[2-9]",bounds=(11,11)))
    display(board)
    play(board,players)

  }
}
