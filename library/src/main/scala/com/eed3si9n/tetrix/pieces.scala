package com.eed3si9n.tetrix

import com.eed3si9n.tetrix.view.Grid

class pieces { }

sealed trait PieceKind{
  /** left bottom corner of each block? */
  val basicPos: Seq[(Int, Int)]
}
case object IKind extends PieceKind {
  override val basicPos: Seq[(Int, Int)] = Seq((0, 0), (1, 0), (2, 0), (3, 0))
}
case object JKind extends PieceKind {
  override val basicPos: Seq[(Int, Int)] = Seq((0, 1), (1, 1), (2, 1), (2, 0))
}
case object LKind extends PieceKind {
  override val basicPos: Seq[(Int, Int)] = Seq((2, 1), (1, 1), (0, 1), (0, 0))
}
case object OKind extends PieceKind {
  override val basicPos: Seq[(Int, Int)] = Seq((0, 1), (1, 1), (0, 0), (1, 0))
}
case object SKind extends PieceKind {
  override val basicPos: Seq[(Int, Int)] = Seq((1, 1), (2, 1), (0, 0), (1, 0))
}
case object TKind extends PieceKind {
  override val basicPos: Seq[(Int, Int)] = Seq((0, 0), (1, 0), (2, 0), (1, 1))
}
case object ZKind extends PieceKind {
  override val basicPos: Seq[(Int, Int)] = Seq((1, 0), (2, 0), (0, 1), (1, 1))
}
/** Have reference to no existing point. */
case object Dummy extends PieceKind {
  override val basicPos: Seq[(Int, Int)] = Seq()
}

case object PieceKind {
  def apply(x: Int): PieceKind = x match {
    case 0 => IKind
    case 1 => JKind
    case 2 => LKind
    case 3 => OKind
    case 4 => SKind
    case 5 => TKind
    case 6 => ZKind
  }
}

sealed trait GameStatus
case object ActiveStatus extends GameStatus
case object GameOver extends GameStatus

/**
  *
  * @param pos center position
  * @param kind
  */
case class Block(pos: Grid, kind: PieceKind)
case class GameView(blocks: Seq[Block], gridSize: Grid,
    current: Seq[Block], next: Seq[Block], status: GameStatus) {
}

/**
  *
  * @param blocks all blocks in view (includes blocks of `currentPiece`)?
  * @param gridSize
  * @param currentPiece
  * @param nextPiece
  * @param kinds to generate nextPiece
  */
case class GameState(blocks: Seq[Block], gridSize: Grid, currentPiece: Piece,
    nextPiece: Piece, kinds: Seq[PieceKind], status: GameStatus = ActiveStatus) {
  def view: GameView = GameView(blocks, gridSize,
    currentPiece.current, nextPiece.current, status)
  def dropOffPos = (gridSize._1 / 2.0, gridSize._2 - 0.0)
}

// pos: (Int, Int)
//  y
//  ↑
//  .-→ x
// locals := local coordinate
// A `Piece` is composed of `Block`s
case class Piece(pos: (Double, Double), kind: PieceKind, locals: Seq[(Double, Double)]) {
  def current: Seq[Block] = locals map { case (x, y) =>
        Block((math.floor(x + pos._1).toInt, math.floor(y + pos._2).toInt), kind)
    }
  def moveBy(delta: (Double, Double)): Piece =
    copy(pos = (pos._1 + delta._1, pos._2 + delta._2))
  def rotateBy(theta: Double): Piece = {
    val c = math.cos(theta)
    val s = math.sin(theta)
    def roundToHalf(v: (Double, Double)): (Double, Double) =
      (math.round(v._1 * 2.0) * 0.5, math.round(v._2 * 2.0) * 0.5)
    copy(locals = locals map { case(x, y) => (x * c - y *s, y * c + x * s) } map roundToHalf)
  }
}
case object Piece {
  // pos: Center point of each block ?
  def apply(pos: (Double, Double), kind: PieceKind): Piece = kind match {
    case IKind => Piece(pos, kind, Seq((-1.5, 0.0), (-0.5, 0.0), (0.5, 0.0), (1.5, 0.0)))

    /* mirror image along y-axis */
    case JKind => Piece(pos, kind, Seq((-1.0, 0.5), (0.0, 0.5), (1.0, 0.5), (1.0, -0.5)))
    case LKind => Piece(pos, kind, Seq((1.0, 0.5), (0.0, 0.5), (-1.0, 0.5), (-1.0, -0.5)))

    case OKind => Piece(pos, kind, Seq((-0.5, 0.5), (0.5, 0.5), (-0.5, -0.5), (0.5, -0.5)))

    /* mirror image along x-axis */
    case SKind => Piece(pos, kind, Seq((0.0, 0.5), (1.0, 0.5), (-1.0, -0.5), (0.0, -0.5)))
    case ZKind => Piece(pos, kind, Seq((0.0, -0.5), (1.0, -0.5), (-1.0, 0.5), (0.0, 0.5)))

    case TKind => Piece(pos, kind, Seq((-1.0, 0.0), (0.0, 0.0), (1.0, 0.0), (0.0, 1.0)))
    case Dummy => Piece((0, 0), kind, Seq.empty)
  }
}
