package scala

import scala.collection.immutable.{ArraySeq, Map}
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

object isolation_game {
  def main(args:Array[String]): Unit ={

    def initBoard (dim:(Int, Int)) : (ArraySeq[ArraySeq[Any]],Map[String,(Int, Int)]) = {
      val x = dim._1 //8 rows
      val y = dim._2 //6 cols

      val board = ArraySeq.tabulate(x, y){
        case (a, b) =>
          if(y%2 == 0) {
            if(a == 0 && b == y/2){"B"}
            else{
              if(a == x-1 && b == (y/2)-1){"A"}
              else{1}
            }
          }
          else{
            if(a == 0 && b == y/2){"B"}
            else{
              if(a == x-1 && b == y/2){"A"}
              else{1}
            }
          }

      }
      if(y%2 == 0) {
        val players = Map("A" -> (x-1,y/2 - 1), "B" -> (0,y/2))
        return (board,players)
      }
      else{
        val players = Map("A" -> (x-1,y/2), "B" -> (0,y/2))
        return (board,players)
      }
    }

    def display(board:ArraySeq[ArraySeq[Any]]): Unit ={
      println(".  "+List.range(0, board(0).size).mkString("   "))
      println(".  "+List.fill(board(0).size)("_").mkString("   "))
      //  println(board.map(_.mkString("   ")).mkString("\n"))
      board.zipWithIndex.foreach{ case(row, i) => println(i+" |"+row.mkString("   "));}
    }

    def pos(limits:String): (Int,Int) = {
      println("Please select numbers in range "+limits)
      val pattern = limits
      print("Row : ")
      val x = readLine()
      print("Column : ")
      val y = readLine()

      if(!x.matches(pattern) || !y.matches(pattern)){
        print("Wrong inputs\n")
        pos(limits)
      }
      else{
        return (x.toInt,y.toInt)
      }

    }

    def remove_cell(board:ArraySeq[ArraySeq[Any]], player:String) : ArraySeq[ArraySeq[Any]]={
      print("Player "+player+" remove turn\n")
      val (x,y) = pos(limits="[0-9]")
      if(board(0).length%2 == 0) {
        if(board(x)(y) != 1 || (x,y) == (board.length-1,board(0).length/2 - 1) || (x,y) == (0,board(0).length/2)){
          print("Invalid cell selected\n")
          remove_cell(board,player)
        }
        else{
          return board.updated(x, board(x).updated(y, 0))
        }
      }
      else{
        if(board(x)(y) != 1 || (x,y) == (board.length-1,(board(0).length-1)/2) || (x,y) == (0,(board(0).length-1)/2) ){
          print("Invalid cell selected\n")
          remove_cell(board,player)
        }
        else{
          return board.updated(x, board(x).updated(y, 0))
        }
      }

    }

    def move(board:ArraySeq[ArraySeq[Any]], players:Map[String,(Int, Int)], player:String): (ArraySeq[ArraySeq[Any]],Map[String,(Int, Int)])={
      print("Player "+player+" move turn\n")
      val (x,y) = pos(limits="[0-9]")
      if(board(x)(y) != 1){
        print("Invalid cell to move selected : not an available cell\n")
        move(board,players,player)
      }
      else{
        if(player=="A" && (players("A")._1-x <= 1 && players("A")._1-x >= -1) && (players("A")._2-y <= 1 && players("A")._2-y >= -1)){
          val board_removeA = board.updated(players("A")._1, board(players("A")._1).updated(players("A")._2, 1))
          val players_updated = Map("A" -> (x,y), "B" -> players("B"))
          val board_updated = board_removeA.updated(players_updated("A")._1, board_removeA(players_updated("A")._1).updated(players_updated("A")._2, player))
          return (board_updated,players_updated)
        }
        else{
          if(player=="B" && (players("B")._1-x <= 1 && players("B")._1-x >= -1) && (players("B")._2-y <= 1 && players("B")._2-y >= -1)){
            val board_removeA = board.updated(players("B")._1, board(players("B")._1).updated(players("B")._2, 1))
            val players_updated = Map("A" -> players("A"), "B" -> (x,y))
            val board_updated = board_removeA.updated(players_updated("B")._1, board_removeA(players_updated("B")._1).updated(players_updated("B")._2, player))
            return (board_updated,players_updated)
          }
          else{
            print("Invalid cell to move selected : too far\n")
            move(board,players,player)
          }
        }
      }
    }

    def play(board:ArraySeq[ArraySeq[Any]], players: Map[String,(Int, Int)]): Unit = {
      val playerA = "A"
      val playerB = "B"

      check_turn(board,players,playerA,move=true)
      val (board_moveA,players_updateA) = move(board,players,playerA)
      display(board_moveA)

      check_turn(board_moveA,players_updateA,playerA,move=false)
      val board_removeA = remove_cell(board_moveA,playerA)
      display(board_removeA)

      check_turn(board_removeA,players_updateA,playerB,move=true)
      val (board_moveB,players_updateB) = move(board_removeA,players_updateA,playerB)
      display(board_moveB)

      check_turn(board_moveB,players_updateB,playerB,move=false)
      val board_removeB = remove_cell(board_moveB,playerB)
      display(board_removeB)

      play(board_removeB,players_updateB)
    }

    def check_turn(board: ArraySeq[ArraySeq[Any]], players: Map[String, (Int, Int)], player: String, move: Boolean): Unit = {
      val x = players(player)._1
      val y = players(player)._2
      val opponent = Map("A" -> "B", "B" -> "A")
      val bound_moves = ListBuffer[Any]() //mutable list to add every available moves within bounds (example : left move -> (x-1,y+0) only if x-1 >= 0)
      for(i <- -1 to 1) {
        for(j <- -1 to 1){
          if( (x+i > -1) && (x+i < board.length) && (y+j > -1) && (y+j < board(0).length) && ((i,j) != (0,0)) ){ //No out of bound moves + No staying in place move
            if(board(x+i)(y+j) != opponent(player) && board(x+i)(y+j) != 0){ //can't move on opponent and can't move on removed cell
              if(board(0).length%2 == 0){
                if(!(!move && ((x+i,y+j) == (board.length-1,board(0).length/2 - 1) || (x+i,y+j) == (0,board(0).length/2)))) {
                  bound_moves += board(x+i)(y+j)
                }
              }
              else{
                if(!(!move && ((x+i,y+j) == (board.length-1,(board(0).length-1)/2) || (x+i,y+j) == (0,(board(0).length-1)/2)))){
                  bound_moves += board(x+i)(y+j)
                }
              }
            }
          }
        }
      }

      val moves = bound_moves.toList
      //println("moves = ",moves.size,moves.length,moves)

      if(!moves.contains(1)){
        if(moves.isEmpty){
          print("No allowed move available : ")
        }
        println("Player "+opponent(player)+" won")
        System.exit(1)
      }

    }

    val (board,players) = initBoard(pos(limits="[2-9]"))
    display(board)
    play(board,players)

  }
}
