class Board(var squares: Array[Array[Piece]]) extends Serializable {
  if(squares == null)
    squares = Array(
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null))

  def this(board: Board) = {
    this(board.squares.map(_.clone))
  }

  def valueByPieces(): Int = {
    var value = 0
    for(i <- 0 until squares.length; j <- 0 until squares(i).length; if squares(i)(j) != null) {
      value += squares(i)(j).value
    }
    value
  }

  // Console printing for debugging purposes
  def print(): Unit = {
    Console.print("\n  0 1 2 3 4 5 6 7")
    for(i <- 0 until squares.length; j <- 0 until squares(i).length) {
      if(j==0) Console.print("\n" + i)
      Console.print(Console.BOLD)
      if(squares(i)(j) != null) {
        if(squares(i)(j).isMax) Console.print(Console.BLUE)
        else Console.print(Console.GREEN)
        squares(i)(j).pieceType match {
          case "pawn" => Console.print(" p")
          case "knight" => Console.print(" k")
          case "bishop" => Console.print(" b")
          case "rook" => Console.print(" r")
          case "queen" => Console.print(" q")
          case "king" => Console.print(" k")
          case _ => Console.print(" ?")
        }
      } else {
        Console.print(" ?")
      }
      Console.print(Console.RESET)
    }
    Console.print("\n")
  }
}
