package com.eed3si9n.tetrix

package object view {
  type Grid = (Int, Int)
  val Size = (10, 20)
  val MiniGridSize = (4, 2)
  type Blocks = Seq[Block]
  val FRate = 1000 // [msec]
}
