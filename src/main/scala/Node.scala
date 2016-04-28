class Node(val board: Board, val max: Boolean) extends Serializable {
  var value: Int = 0
  var terminal: Boolean = false
  var children: Array[Node] = Array()

  checkTerminal()

  private def checkTerminal(): Unit = {
    var kingMaxAlive = false
    var kingMinAlive = false
    for(i <- 0 until board.squares.length; j <- 0 until board.squares(i).length) {
      if(board.squares(i)(j) != null && board.squares(i)(j).pieceType == Piece.KingMax.pieceType) {
        if(board.squares(i)(j).isMax) kingMaxAlive = true
        else kingMinAlive = true
      }
    }
    if(!kingMaxAlive){
      terminal = true
      value = Piece.KnightMin.value * 2 + Piece.BishopMin.value * 2 + Piece.RookMin.value * 2 + Piece.QueenMin.value * (8 + 1) - 1
    } else if (!kingMinAlive) {
      terminal = true
      value = Piece.KnightMax.value * 2 + Piece.BishopMax.value * 2 + Piece.RookMax.value * 2 + Piece.QueenMax.value * (8 + 1) + 1
    } else if (checkDraw()) {
      terminal = true
    }
  }

  // Basic draw checking (insufficient material method)
  // TODO Case: Only bishops for both players at the same color position
  private def checkDraw(): Boolean = {
    var maxOnlyKing = true
    var maxOnlyKnight = true
    var minOnlyKing = true
    var minOnlyKnight = true
    for(i <- 0 until board.squares.length; j <- 0 until board.squares(i).length) {
      if(board.squares(i)(j) != null && board.squares(i)(j).isMax && board.squares(i)(j).pieceType != Piece.KingMax.pieceType) {
        maxOnlyKing = false
        if(board.squares(i)(j).pieceType != Piece.KnightMax.pieceType){
          maxOnlyKnight = false
        }
      }
      else if(board.squares(i)(j) != null && !board.squares(i)(j).isMax && board.squares(i)(j).pieceType != Piece.KingMin.pieceType) {
        minOnlyKing = false
        if(board.squares(i)(j).pieceType != Piece.KnightMin.pieceType){
          minOnlyKnight = false
        }
      }
    }
    maxOnlyKing || minOnlyKing || maxOnlyKnight && minOnlyKnight
  }

  // Possible movements for the player indicated by the max attribute
  def boardMovements(): Array[Board] = {
    var boardMovements: Array[Board] = Array()
    for(i <- 0 until board.squares.length; j <- 0 until board.squares(i).length;
        if board.squares(i)(j) != null && board.squares(i)(j).isMax == max) {
      for(move <- board.squares(i)(j).movements(Array(i,j), board.squares)) {
        val boardMove = new Board(board)
        boardMove.squares(move(0))(move(1)) = boardMove.squares(i)(j)
        boardMove.squares(i)(j) = null
        boardMovements = boardMovements :+ boardMove
      }
    }
    boardMovements
  }

  def buildTree(levels: Int, firstLevel: Boolean = true): Array[Int] = {
    var quantityChildren = 0
    if(!terminal && levels > 0) {
      var firstIteration = true
      if(children.isEmpty) children = boardMovements().map(new Node(_, !max))
      for(nodeChild <- children) {
        val valuesChild = nodeChild.buildTree(levels - 1, false)
        val valueChild = valuesChild(0)
        quantityChildren += valuesChild(1)
        if(max && (value < valueChild) || !max && (value > valueChild) || firstIteration) {
          value = valueChild
          firstIteration = false
        }
      }
      // Deallocate memory pruning calculated children
      if(!firstLevel) children = Array()
    } else if(!terminal) {
      value = board.valueByPieces
    }
    /*DEBUG if(levels == 3){board.print();println("");println("terminal:"+terminal);println("value:"+value);println("max:"+max)}*/
    Array(value, quantityChildren + 1)
  }
}
