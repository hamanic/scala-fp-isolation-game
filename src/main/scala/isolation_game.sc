def initBoard (x:Int, y:Int) : Array[Array[Any]] = {
  val rows = x //rows
  val cols = y //cols
  val board = Array.ofDim[Any](rows, cols)

  for {
    i <- 0 until x
    j <- 0 until y
  } board(i)(j) = 1

  board(0)(3) = "B"
  board(7)(2) = "A"

  return board
}

val board = initBoard(8,6)
board(0)(0)

//val board: List[List[Any]] =
//  List(
//    List(1, 1, 1,"B", 1, 1),
//    List(1, 1, 1, 1, 1, 1),
//    List(1, 1, 1, 1, 1, 1),
//    List(1, 1, 1, 1, 1, 1),
//    List(1, 1, 1, 1, 1, 1),
//    List(1, 1, 1, 1, 1, 1),
//    List(1, 1, 1, 1, 1, 1),
//    List(1, 1, "A", 1, 1, 1),
//  )

board(7)(2)

def move(player:String, pos:Int) : Array[Array[String]]={
  if(turn == 2){
    println("select board position to remove")
    return Unit
  }
  else{
    println("Select position to move")
  }
}

def win(b:List[List[Any]]): Boolean ={

}



def play(b:Array[Array[String]])={
  if win(){ //boolean + winner
    return
  }
  play()
}