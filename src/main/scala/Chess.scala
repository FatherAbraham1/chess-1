import org.apache.spark.{SparkConf, SparkContext}

object Chess {

  /*
   * args(0): depthLevels
   */
  def main(args: Array[String]): Unit = {
    val depthLevels = if(args.size > 0) args(0).toInt else 5

    val initialBoardSquares: Array[Array[Piece]] = Array(
      Array(Piece.RookMax,Piece.KnightMax,Piece.BishopMax,Piece.QueenMax,Piece.KingMax,Piece.BishopMax,Piece.KnightMax,Piece.RookMax),
      Array(Piece.PawnMax,Piece.PawnMax,Piece.PawnMax,Piece.PawnMax,Piece.PawnMax,Piece.PawnMax,Piece.PawnMax,Piece.PawnMax),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(null,null,null,null,null,null,null,null),
      Array(Piece.PawnMin,Piece.PawnMin,Piece.PawnMin,Piece.PawnMin,Piece.PawnMin,Piece.PawnMin,Piece.PawnMin,Piece.PawnMin),
      Array(Piece.RookMin,Piece.KnightMin,Piece.BishopMin,Piece.QueenMin,Piece.KingMin,Piece.BishopMin,Piece.KnightMin,Piece.RookMin)
    )

    // Initialize SparkContext
    val sc = new SparkContext(new SparkConf().setAppName("Chess"))

    var root = new NodeRDD(
      new Board(initialBoardSquares),
      false,
      sc.emptyRDD
    )

    do{
      if(root.max) root = makeMaxMove(root, sc)
      else root = changeRoot(root, readMinMove(root.board), sc, depthLevels)
    }while(!root.terminal)

    // End game console printing for debugging purposes
    root.board.print()
    Console.print("Fin. ")
    if(root.value > 0) Console.println ("El programa gana.")
    else if(root.value < 0) Console.println ("El usuario gana.")
    else Console.println ("Empate.")
  }

  def makeMaxMove(node: NodeRDD, sc: SparkContext): NodeRDD = {
    var accNode: Node = null
    for(i <- node.childrenRDD.collect(); if accNode == null || accNode.value <= i._1.value) accNode = i._1
    // Console printing for debugging purposes
    Console.println("Valor max: " + accNode.value)
    new NodeRDD(accNode,sc.emptyRDD)
  }

  // Console read for debugging purposes
  def readMinMove(board: Board): Board = {
    var currentPosition: String = null
    var newPosition: String = null

    board.print()
    do {
      currentPosition = readLine("Escriba posición actual de la pieza (columnrow):")
      newPosition = readLine("Escriba nueva posición de la pieza (columnrow):")
    // TODO Improve legality checking of the movement
    }while(board.squares(currentPosition.charAt(1).asDigit)(currentPosition.charAt(0).asDigit) == null
      || board.squares(currentPosition.charAt(1).asDigit)(currentPosition.charAt(0).asDigit).isMax
      || (board.squares(newPosition.charAt(1).asDigit)(newPosition.charAt(0).asDigit) != null
        && !board.squares(newPosition.charAt(1).asDigit)(newPosition.charAt(0).asDigit).isMax))

    val newBoard = new Board(board)

    newBoard.squares(newPosition.charAt(1).asDigit)(newPosition.charAt(0).asDigit) =
      newBoard.squares(currentPosition.charAt(1).asDigit)(currentPosition.charAt(0).asDigit)

    newBoard.squares(currentPosition.charAt(1).asDigit)(currentPosition.charAt(0).asDigit) = null

    newBoard
  }

  def changeRoot(oldRoot: NodeRDD, newBoard: Board, sc: SparkContext, depthLevels: Int): NodeRDD = {
    var newRoot: NodeRDD = null

    if(!oldRoot.children.isEmpty) {
      for(i <- oldRoot.children; if i.board.squares.deep == newBoard.squares.deep)
        newRoot = new NodeRDD(i, sc.parallelize(i.boardMovements().map(board => (new Node(board, !i.max), 0))))
    }

    if(newRoot == null)  {
      newRoot = new NodeRDD(
        new Board(newBoard),
        !oldRoot.max,
        sc.parallelize(
          new Node(new Board(newBoard), !oldRoot.max).boardMovements().map(board => (new Node(board, oldRoot.max), 0))
        )
      )
    }

    val quantityChildren = newRoot.buildTreeRDD(depthLevels)

    // Console printing for debugging purposes
    Console.println(quantityChildren + " posibles jugadas calculadas.")

    newRoot
  }
}