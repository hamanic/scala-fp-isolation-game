package scala

import java.io._
import scala.collection.immutable.{ArraySeq, Vector}
import scala.io.StdIn.readLine
import scala.math.{ceil, floor}

object isolation_game {
  def main(args:Array[String]): Unit ={

    def initBoard (x:Int, y:Int) : ArraySeq[ArraySeq[Any]] = {
      val rows = x //8 rows
      val cols = y //6 cols

      if(y%2 == 0) {
        val posA = (x-1,floor(y/2).toInt-1)
        val posB = (0,ceil(y/2).toInt)
      }
      else{
        val posA = (x-1,y/2)
        val posB = (0,y/2)
      }

      val board = ArraySeq.tabulate(rows, cols){
        case (a, b) =>
          if(y%2 == 0) {
            if(a == 0 && b == ceil(y/2).toInt){
              "B"
            }
            else{
              if(a == x-1 && b == floor(y/2).toInt-1){
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
      return board
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

    val (x,y) = pos("[4-9]")
    val board = initBoard(x,y)
    display(board)
    display(remove_cell(board))
  }
}
