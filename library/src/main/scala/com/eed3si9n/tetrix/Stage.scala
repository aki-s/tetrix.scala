package com.eed3si9n.tetrix

import com.typesafe.scalalogging.Logger

import scala.annotation.tailrec

object Stage {
  val logger = Logger("Stage")

  def randomStream(random: util.Random): Stream[PieceKind] =
    PieceKind(random.nextInt(7)) #:: randomStream(random)

  /** Generate new game state.
    *
    * @param blocks pre existing blocks
    * @param gridSize stage size
    * @param kinds kinds of future pieces in tern.
    *              There must be 2 elements for current and next piece at least.
    *              There must be 3 elements when next piece becomes current.
    * @return
    */
  def newState(blocks: Seq[Block], gridSize: view.Grid, kinds: Seq[PieceKind]): GameState = {
    val dummy = Piece((0, 0), Dummy) // TODO: change to pre-defined DummyPiece object
    spawn(spawn(GameState(Nil, gridSize, dummy, dummy, kinds)).copy(blocks = blocks))
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
      val next = Piece(s.dropOffPos, s.kinds.head)
      val spawned = s.nextPiece
      val s1 = s.copy(blocks = s.blocks,
        currentPiece = spawned,
        nextPiece = next,
        kinds = s.kinds.tail
      )
      validate(s1) map { x =>
        x.load(x.currentPiece)
      } getOrElse {
        s1.load(s1.currentPiece).copy(status = GameOver)
      }
    }

  /** Clear a row if it is full.
    *
    * - Check row `y` (>=0) is full
    * - Scan range from `y` to 0, then clear each row if it is full
    * - When row `y` is cleared, shift all blocks above `y` downward
    * - Count up by 1 if row is cleared
    */
  private[this] lazy val clearFullRow: GameState => GameState =
  (s0: GameState) => {
    /** Check row `y` is full.  */
    def isFullRow(y: Int, s: GameState): Boolean =
    s.blocks.count (_.pos._2 == y) == s.gridSize._1

    /** Scan range from `y` to 0, then clear each row if it is full. */
    @tailrec
    def tryRow(y: Int, s: GameState): GameState =
    if (y < 0) s
    else if (isFullRow(y, s))
    // When row `y` is cleared, shift all blocks above `y` downward.
      tryRow(y - 1, s.copy(blocks = (s.blocks filter {_.pos._2 < y})
        ++ (s.blocks filter {_.pos._2 > y} map { b => b.copy(pos = (b.pos._1, b.pos._2 - 1))}),
        lineCount = s.lineCount + 1))
    else tryRow(y - 1, s)

    tryRow(s0.gridSize._2 - 1, s0)
  }

  /** Transit game status. */
  private[this] def transit(trans: Piece => Piece,
      onFail: GameState => GameState = identity
  ): GameState => GameState = {
    (s0: GameState) =>
      s0.status match {
        case ActiveStatus =>
          val sTmp = s0.unload(s0.currentPiece).copy(
            currentPiece = trans(s0.currentPiece))
          validate(sTmp) map { x =>
            val s1 = x.load(x.currentPiece)
            logger.debug(s"""
                            |Try to transit from `$s0`
                            |                 to `$s1` for current blocks `${s0.currentPiece.current}`""".stripMargin)
            s1
          } getOrElse {
            logger.debug(s"Transit failed for `$s0`")
            onFail(s0)
          }
        case GameOver =>
          logger.debug("No transit because of GameOver.")
          s0
      }
  }
  /** Return None if collision happened. */
  private[this] def validate(s: GameState): Option[GameState] = {
    val size = s.gridSize
    def inBounds(pos: (Int, Int)): Boolean =
      (0 <= pos._1) && (pos._1 < size._1) && (0 <= pos._2)
    val currentPoss = s.currentPiece.current map {_.pos}
    if ((currentPoss forall inBounds) &&
      (s.blocks map {_.pos} intersect currentPoss).isEmpty) Some(s)
    else None
  }


  private[tetrix] val possibleMoves: Seq[StageMessage] =
    Seq(MoveLeft, MoveRight, RotateCW, Tick, Drop)
  private[tetrix] def toTrans(message: StageMessage): GameState => GameState = message match {
    case MoveLeft  => moveLeft
    case MoveRight => moveRight
    case RotateCW  => rotateCW
    case Tick      => tick
    case Drop      => drop
  }

}

