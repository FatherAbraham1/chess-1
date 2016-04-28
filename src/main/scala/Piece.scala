class Piece (val isMax: Boolean, val pieceType: String) extends Serializable {
  val value = pieceType match {
    case "pawn" => if(isMax) 1 else -1
    case "knight" => if(isMax) 3 else -3
    case "bishop" => if(isMax) 3 else -3
    case "rook" => if(isMax) 5 else -5
    case "queen" => if(isMax) 9 else -9
    case _ => 0
  }

  def movements(position: Array[Int], squares: Array[Array[Piece]]): Array[Array[Int]] = {
    var movements: Array[Array[Int]] = Array()
    pieceType match {
      case "pawn" =>
        if(isMax) {
          movements = addMovement(movements, Array(position(0) + 1, position(1)), squares, false, true)
          if (position(0) == 1) movements = addMovement(movements, Array(position(0) + 2, position(1)), squares, false, true)
          movements = addMovement(movements, Array(position(0) + 1, position(1) - 1), squares, true)
          movements = addMovement(movements, Array(position(0) + 1, position(1) + 1), squares, true)
        } else {
          movements = addMovement(movements, Array(position(0) - 1, position(1)), squares, false, true)
          if (position(0) == 6) movements = addMovement(movements, Array(position(0) - 2, position(1)), squares, false, true)
          movements = addMovement(movements, Array(position(0) - 1, position(1) - 1), squares, true)
          movements = addMovement(movements, Array(position(0) - 1, position(1) + 1), squares, true)
        }
      case "knight" =>
        movements = addMovement(movements, Array(position(0) + 2, position(1) + 1), squares)
        movements = addMovement(movements, Array(position(0) + 2, position(1) - 1), squares)
        movements = addMovement(movements, Array(position(0) + 1, position(1) + 2), squares)
        movements = addMovement(movements, Array(position(0) - 1, position(1) + 2), squares)
        movements = addMovement(movements, Array(position(0) - 2, position(1) + 1), squares)
        movements = addMovement(movements, Array(position(0) - 2, position(1) - 1), squares)
        movements = addMovement(movements, Array(position(0) + 1, position(1) - 2), squares)
        movements = addMovement(movements, Array(position(0) - 1, position(1) - 2), squares)
      case "bishop" =>
        movements = addMovement(movements, Array(position(0) - 1, position(1) + 1), squares, false, false, Array(-1, 1))
        movements = addMovement(movements, Array(position(0) - 1, position(1) - 1), squares, false, false, Array(-1, -1))
        movements = addMovement(movements, Array(position(0) + 1, position(1) - 1), squares, false, false, Array(1, -1))
        movements = addMovement(movements, Array(position(0) + 1, position(1) + 1), squares, false, false, Array(1, 1))
      case "rook" =>
        movements = addMovement(movements, Array(position(0) + 1, position(1)), squares, false, false, Array(1, 0))
        movements = addMovement(movements, Array(position(0) - 1, position(1)), squares, false, false, Array(-1, 0))
        movements = addMovement(movements, Array(position(0), position(1) + 1), squares, false, false, Array(0, 1))
        movements = addMovement(movements, Array(position(0), position(1) - 1), squares, false, false, Array(0, -1))
      case "queen" =>
        movements = addMovement(movements, Array(position(0) - 1, position(1) + 1), squares, false, false, Array(-1, 1))
        movements = addMovement(movements, Array(position(0) - 1, position(1) - 1), squares, false, false, Array(-1, -1))
        movements = addMovement(movements, Array(position(0) + 1, position(1) - 1), squares, false, false, Array(1, -1))
        movements = addMovement(movements, Array(position(0) + 1, position(1) + 1), squares, false, false, Array(1, 1))
        movements = addMovement(movements, Array(position(0) + 1, position(1)), squares, false, false, Array(1, 0))
        movements = addMovement(movements, Array(position(0) - 1, position(1)), squares, false, false, Array(-1, 0))
        movements = addMovement(movements, Array(position(0), position(1) + 1), squares, false, false, Array(0, 1))
        movements = addMovement(movements, Array(position(0), position(1) - 1), squares, false, false, Array(0, -1))
      case "king" =>
        for(i <- position(0) - 1 to position(0) + 1;
            j <- position(1) - 1 to position(1) + 1;
            if i != position(0) && j != position(1))
            movements = addMovement(movements, Array(i, j), squares)
      case _ =>
    }
    movements
  }

  private def insideBoard(position: Array[Int], squares: Array[Array[Piece]]) : Boolean = {
    val boardRange = 0 until squares.length
    (boardRange contains position(0)) && (boardRange contains position(1))
  }

  // TODO Note that not checking for a check in a king move
  private def addMovement(movements: Array[Array[Int]], movement: Array[Int], squares: Array[Array[Piece]],
                          isPawnDiagonal: Boolean = false, isPawnFront: Boolean = false,
                          vectorForRecursive: Array[Int] = null): Array[Array[Int]] = {
    var newMovements = movements
    if(insideBoard(movement, squares)
      && (squares(movement(0))(movement(1)) == null && !isPawnDiagonal
        || squares(movement(0))(movement(1)) != null && squares(movement(0))(movement(1)).isMax != isMax && !isPawnFront)) {
      newMovements = newMovements :+ movement

      if (vectorForRecursive != null && squares(movement(0))(movement(1)) == null)
        newMovements = addMovement(newMovements, Array(movement(0) + vectorForRecursive(0), movement(1) + vectorForRecursive(1)),
                                    squares, isPawnDiagonal, isPawnFront, vectorForRecursive)
    }

    newMovements
  }
}

object Piece {
  val PawnMax = new Piece(true, "pawn")
  val KnightMax = new Piece(true, "knight")
  val BishopMax = new Piece(true, "bishop")
  val RookMax = new Piece(true, "rook")
  val QueenMax = new Piece(true, "queen")
  val KingMax = new Piece(true, "king")

  val PawnMin = new Piece(false, "pawn")
  val KnightMin = new Piece(false, "knight")
  val BishopMin = new Piece(false, "bishop")
  val RookMin = new Piece(false, "rook")
  val QueenMin = new Piece(false, "queen")
  val KingMin = new Piece(false, "king")
}