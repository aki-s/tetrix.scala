package com.eed3si9n.tetrix

import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

object Stage {
  val logger = Logger("Stage")

  def randomStream(random: util.Random): Stream[PieceKind] =
    PieceKind(random.nextInt(7)) #:: randomStream(random)

  def newState(blocks: Seq[Block], gridSize: view.Grid, kinds: Seq[PieceKind]): GameState = {
    val dummy = Piece((-1, -1), TKind)
    spawn(spawn(GameState(Nil, gridSize, dummy, dummy, kinds)).copy(blocks = blocks))
  }

  @deprecated("", "day3")
  def newState(blocks: Seq[Block]): GameState = {
    val size = view.Size
    def dropOffPos = (size._1 / 2.0, size._2 - 2.0) // Center pos of a piece // index start with 0
    val p = Piece(dropOffPos, TKind)
    val next = Piece(dropOffPos, TKind) // fixme dummy
    GameState(blocks ++ p.current, size, p, next, Seq(next.kind)) // fixme kind & piece
  }

  def moveLeft = transit { _.moveBy(-1.0, 0.0) }
  def moveRight = transit { _.moveBy(1.0, 0.0) }
  def rotateCW = transit { _.rotateBy(- math.Pi / 2.0) }
  def tick = transit(_.moveBy(0.0, -1.0), Function.chain(Seq(clearFullRow, spawn)))
  val moveDown = transit { _.moveBy(0.0, -1.0) }
  val drop: GameState => GameState = (s0: GameState) =>
    Function.chain((Nil padTo (s0.gridSize._2, moveDown)) ++ List(tick))(s0)

  private[this] lazy val spawn: GameState => GameState =
    (s: GameState) => {
      def dropOffPos = (s.gridSize._1 / 2.0, s.gridSize._2 - 2.0)
      val next = Piece(dropOffPos, s.kinds.head)
      val p = s.nextPiece
      s.copy(blocks = s.blocks ++ p.current, currentPiece = p,
        nextPiece = next, kinds = s.kinds.tail)
    }

  private[this] lazy val clearFullRow: GameState => GameState =
    (s0: GameState) => {
      def isFullRow(y: Int, s: GameState): Boolean =
        s.blocks.count (_.pos._2 == y) == s.gridSize._1
      @tailrec def tryRow(y: Int, s: GameState): GameState =
        if (y < 0) s
        else if (isFullRow(y, s))
          tryRow(y - 1, s.copy(blocks = (s.blocks filter {_.pos._2 < y})
            ++ (s.blocks filter {_.pos._2 > y} map { b => b.copy(pos = (b.pos._1, b.pos._2 - 1))})))
        else tryRow(y - 1, s)
      tryRow(s0.gridSize._2 - 1, s0)
    }

  private[this] def transit(trans: Piece => Piece,
      onFail: GameState => GameState = identity
  ): GameState => GameState = {
    (s0: GameState) =>
      val s1 = s0.copy(
        blocks = unload(s0.currentPiece, s0.blocks),
        currentPiece = trans(s0.currentPiece))
      validate(s1) map {
        case x =>
          logger.debug(s"${x.currentPiece} => ${x.currentPiece.current}|${x.blocks}")
          x.copy(blocks = load(x.currentPiece, x.blocks))
      } getOrElse { onFail(s0) }
  }
  /** Return None if collision happened. */
  private[this] def validate(s: GameState): Option[GameState] = {
    val size = s.gridSize
    def inBounds(pos: (Int, Int)): Boolean =
      (0 <= pos._1) && (pos._1 < size._1) && (0 <= pos._2) && (pos._2 < size._2)
    val currentPoss = s.currentPiece.current map {_.pos}
    if ((currentPoss forall inBounds) &&
      (s.blocks map {_.pos} intersect currentPoss).isEmpty) Some(s)
    else None
  }

  /** Unload current blocks of `p` from `bs`. */
  private def unload(p: Piece, bs: Seq[Block]): Seq[Block] = {
    val currentPos = p.current map {_.pos}
    bs filterNot { currentPos contains _.pos }
  }
  /** Load current blocks of `p` into `bs`. */
  private def load(p: Piece, bs: Seq[Block]): Seq[Block] = bs ++ p.current
}

