package com.eed3si9n.util

import com.typesafe.scalalogging.StrictLogging

object Profiler extends StrictLogging {

  def stopWatch[A](name: String = "stopwatch")(arg: => A): A = {
    val t0 = System.currentTimeMillis
    val ret: A = arg
    val t1 = System.currentTimeMillis
    val msg = s"$name took ${t1 - t0} ms"
    println(msg)
    logger.debug(msg)
    ret
  }

}
