package com.aki.util

import com.typesafe.scalalogging.Logger

import scala.collection.mutable

/** Named logger mapper for Logback between loggers defined in logback.xml and class files.
  *
  * - To avoid unintended remains of logger.
  * - Implicit logger `root` is not considered.
  *
  * [Usage] {{{
  *   - case 1: Define managed logger (validation works for this type.)
  *   val devlogger = LoggerManager.dev.debug("Directly access public logger.")
  *   //Note: Assignment at only one location per class makes management super easy.
  *
  *   - case 2: Define runtime logger (validation doesn't work for this type.)
  *   LoggerManager.get("logger-name-as-key")
  * }}}
  */
object LoggerManager {
  val loggerMap: mutable.Map[String, Logger] = mutable.Map.empty
  val loggerSet: mutable.Set[String] = mutable.Set.empty

  /** loggers for production purpose. */
  // undef

  /* todo: overcome lazy initialization of field of object inside object such that,
  * object production { val lazyLogger = generate("scala-2.11.8") }
  */

  /** loggers for development purpose. */
  val dev = generate("dev")

  /** Generate logger under management.
    *
    * @param name
    * @return
    */
  private def generate(name: String): Logger = {
    if (loggerSet.contains(name)) throw new IllegalArgumentException(s"Logger named $name is already defined")
    // TODO: get line and class where logger is defined.
    val l = Logger(name)
    loggerMap += (name -> l)
    loggerSet += name
    l
  }

  /** Get or generate logger by `name`.
    *
    * @param name name of logger to be under managed
    * @return
    */
  def get(name: String): Logger = {
    loggerMap.getOrElse(name, generate(name))
  }

  /** Compare `that` set of logger name with this one.
    *
    * @param that set of logger name
    * @return (Unused loggers which is defined in class file,
    *         Unused loggers which is defined in logback.xml)
    */
  def validate(that: Set[String]): (Set[String], Set[String]) = {
    val valid = loggerSet.intersect(that)
    (loggerSet.toSet &~ that, that &~ loggerSet)
  }

}
