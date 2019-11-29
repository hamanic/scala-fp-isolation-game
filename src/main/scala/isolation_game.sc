import scala.collection.immutable.ArraySeq
import scala.io.StdIn.readInt
import scala.math._

//def initBoard (x:Int, y:Int) : Array[Array[Any]] = {
//  val rows = x //rows
//  val cols = y //cols
//  val board = Array.ofDim[Any](rows, cols)
//
//  for {
//    i <- 0 until x
//    j <- 0 until y
//  } board(i)(j) = 1
//
//  board(0)(3) = "B"
//  board(7)(2) = "A"
//
//  return board
//}


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

def move(board:ArraySeq[ArraySeq[Any]], player:String, to : Unit => (Int,Int) ) : ArraySeq[ArraySeq[Any]]={
  return board
}


def remove_cell(board:ArraySeq[ArraySeq[Any]], cell : Unit => (Int,Int)) : ArraySeq[ArraySeq[Any]]={
  return board
}


def pos(): (Int,Int) = {
  println("Row : ")
  var x = readInt()
  println("Column : ")
  val y: Int = readInt()
  return (x,y)
}

val board = initBoard(3,3)
display(board)
print(board.length,board(0).length,3/2)

//
//def win(b:List[List[Any]]): Boolean ={
//
//}
//
//
//
//def play(b:Array[Array[String]])={
//  if win(){ //boolean + winner
//    return
//  }
//  play()
//}

//val x = pos()
//val Board = ArraySeq.fill(8,6)(1)
//val Board2 = Board.updated(0, Board(0).updated(0, "B"))