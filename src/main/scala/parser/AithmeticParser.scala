package parser

/**
  * 四則演算パーサ
  *
  * ノードツリーを作成し、再帰的に四則演算を行うパーサ
  *
  * TODO ノードツリーの生成
  *
  */
class ArithmeticParser {

  val symbols = Array("+", "-", "*", "/")

  /**
    * 対象の式をパースする
    *
    * @param statement 対象式
    */
  def read(statement: String) = {
    val len = statement.length
    val sState = new StatementState(0, 0, false)
    for (i <- 0 to len) {
      sState.endIdx = i
      symbols.foreach(sym => {
        if (sym.equals(statement.charAt(i))) {

          if (sState.isRight) {

          } else {
            sState.enqueue(statement.substring(sState.startIdx, sState.endIdx))
            sState.startIdx = i + 1
          }
        }
      })

    }
  }
}

sealed trait Node {
  var node: Any
}

/**
  * 終端ノードクラス
  *
  * @param node
  */
case class Leaf(override val node: Int) extends Node {
  def +(target: Leaf): Leaf = Leaf(this.node + target.node)
  def -(target: Leaf): Leaf = Leaf(this.node - target.node)
  def *(target: Leaf): Leaf = Leaf(this.node * target.node)
  def /(target: Leaf): Leaf = Leaf(this.node / target.node)
}

/**
  * 入れ子ノードクラス
  *
  * @param node
  */
case class Stem(override val node: NodeManager) extends Node

/**
  * ノード管理クラス
  *
  * @param left
  * @param right
  * @param symbol
  */
class NodeManager(var left: Node, var right: Node, val symbol: String) {

  /**
    * Leafの計算を行う関数
    */
  var fnCalc: (Node, Node) => Leaf = defnCalc(symbol)

  /**
    * 記号から計算用の関数を定義する
    *
    * @param symbol
    * @return
    */
  private def defnCalc(symbol: String): (Node, Node) => Leaf = {
    symbol match {
      case "+" => (left: Node, right: Node) => Leaf(left.asInstanceOf[Leaf].node + right.asInstanceOf[Leaf].node)
      case "-" => (left: Node, right: Node) => Leaf(left.asInstanceOf[Leaf].node - right.asInstanceOf[Leaf].node)
      case "*" => (left: Node, right: Node) => Leaf(left.asInstanceOf[Leaf].node * right.asInstanceOf[Leaf].node)
      case "/" => (left: Node, right: Node) => Leaf(left.asInstanceOf[Leaf].node / right.asInstanceOf[Leaf].node)
      // TODO どのパターンでもない場合、例外
    }
  }

  /**
    * 式の評価を行う
    */
  def evaluate(): Unit = {
    if (isStem(left)) {
      right.asInstanceOf[Stem].node.evaluate
    }
    if (isStem(right)) {
      right.asInstanceOf[Stem].node.evaluate
    }
    if (isCalcOk(left, right)) {
      return fnCalc(right, left)
    }
  }

  def isCalcOk(right: Node, left: Node): Boolean = isLeaf(right) && isLeaf(left)
  def isLeaf(node: Node): Boolean = node.isInstanceOf[Leaf]
  def isStem(node: Node): Boolean = node.isInstanceOf[Stem]

}

/**
  * ノードツリーのルートとなるNodeManaagerの管理クラス
  *
  * @param nodeManager
  */
class RootManager(var nodeManager: NodeManager)

/**
  * 対象式のポインタ状態を管理する
  *
  * @param startIdx 開始点
  * @param endIdx 現在点
  * @param isRight 右辺判定
  */
class StatementState(var startIdx: Int, var endIdx: Int, var isRight: Boolean) {

  // 読み取った数値を格納するキュー
  private var numQueue = new scala.collection.mutable.Queue[Int]
  // エンキュー
  def enqueue(in: String): Unit = numQueue += in.trim.toInt
  // キューの内容をリストで取り出す
  def dequeueAll: List[Int] = numQueue.toList
}

