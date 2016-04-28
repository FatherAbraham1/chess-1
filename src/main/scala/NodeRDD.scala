import org.apache.spark.rdd.RDD

class NodeRDD(board: Board, max: Boolean, var childrenRDD: RDD[(Node, Int)]) extends Node(board, max) {
  def this(node: Node, childrenRDD: RDD[(Node, Int)]) = {
    this(node.board, node.max, childrenRDD)
    value = node.value
    children = node.children
  }

  def buildTreeRDD(levels: Int): Int = {
    childrenRDD = childrenRDD.map(nodeChild => (nodeChild._1, nodeChild._1.buildTree(levels - 1)(1)))
    childrenRDD.map(_._2).reduce(_ + _)
  }
}