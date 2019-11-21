def initBoard (x:Int, y:Int) : Array[Array[Char]] = {
  val rows = x //rows
  val cols = y //cols
  val board = Array.ofDim[Char](rows, cols)

  for {
    i <- 0 until x
    j <- 0 until y
  } board(i)(j) = '1'

  board(0)(3) = 'B'
  board(7)(2) = 'A'

  return board
}

//def move(player:Char, dir:Int) : Array[Array[Char]]={
//
//}
//
//def play

val board = initBoard(8,6)



println(board(0)(0))