package scala

import java.io._
import scala.collection.immutable.{ArraySeq, Map}
import scala.io.StdIn.readLine
import scala.math.{ceil, floor}

object isolation_game {
  def main(args:Array[String]): Unit ={

    def initBoard (dim:(Int, Int)) : (ArraySeq[ArraySeq[Any]],Map[String,(Int, Int)]) = {
      val x = dim._1 //8 rows
      val y = dim._2 //6 cols

      val board = ArraySeq.tabulate(x, y){
        case (a, b) =>
          if(y%2 == 0) {
            if(a == 0 && b == y/2){
              "B"
            }
            else{
              if(a == x-1 && b == (y/2)-1){
                "A"
              }
              else{
                1
              }
            }
          }
          else{
            if(a == 0 && b == y/2){
              "B"
            }
            else{
              if(a == x-1 && b == y/2){
                "A"
              }
              else{
                1
              }
            }
          }

      }
      if(y%2 == 0) {
        val players = Map("A" -> (x-1,floor(y/2).toInt-1), "B" -> (0,ceil(y/2).toInt))
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
      //print("Please select numbers between 4 and 9\n")
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
    def remove_cell(board:ArraySeq[ArraySeq[Any]]) : ArraySeq[ArraySeq[Any]]={
      val (x,y) = pos("[0-9]")
      if(board(x)(y) != 1 ){
        print("Invalid cell selected\n")
        remove_cell(board)
      }
      else{
        return board.updated(x, board(x).updated(y, 0))
      }
    }

    val (board,players) = initBoard(pos("[4-9]"))
    println(players)
    display(board)
    display(remove_cell(board))

//    def play(board:ArraySeq[ArraySeq[Any]],players: Map[String,(Int, Int)]): Unit = {
//      if(check_win()){ print("end") return Unit}
//      else{
//          board_moveA = move(board,playerA))
//          display(board_moveA)
//          board_removeA = remove_cell(board_moveA))
//          display(board_removeA)

//          board_moveB = move(board_removeA,playerA))
//          display(board_moveB)
//          board_removeB = remove_cell(board_moveB))
//          display(board_removeB)

//          play(board_removeB,players)
//      }
//    }
//      def move(board:ArraySeq[ArraySeq[Any]], posPlayer: (Int,Int)): ArraySeq[ArraySeq[Any]]={
//
//    }
  }
}
