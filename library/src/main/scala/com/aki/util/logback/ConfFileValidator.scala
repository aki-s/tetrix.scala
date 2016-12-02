package com.aki.util.logback

import java.net.URL

import ch.qos.logback.classic.spi.LoggerContextListener
import ch.qos.logback.classic.util.ContextInitializer
import ch.qos.logback.classic.{Level, LoggerContext, Logger => LBLogger}
import ch.qos.logback.core.Context
import ch.qos.logback.core.joran.event.{SaxEvent, SaxEventRecorder, StartEvent}
import ch.qos.logback.core.spi.ContextAwareBase
import ch.qos.logback.core.util.Loader
import com.aki.util.LoggerManager

import scala.collection.JavaConverters._
import scala.collection.mutable

/** Reference validator for usage of logger name of logback and class file.
  *
  * Supported conf file is only
  * [[ch.qos.logback.classic.util.ContextInitializer.AUTOCONFIG_FILE]]
  *
  */
class ConfFileValidator
/** LoggerContextListenerAction#begin calls ContextAwareBase#setContext.
  * - #addInfo like such is useful to track process.
  *   - #addInfo is ignored if no context is set.
  */
  extends ContextAwareBase
    /** Works as a hook to be recognized by Logback in logback.xml. */
    with LoggerContextListener {

  /* Delay init until `context` is set. */

  private lazy val saxEvents: Seq[SaxEvent] = parseXml(logbackUrl)
  private lazy val loggerSet: Set[String] = createLoggerSet(saxEvents)
  private val ConfigFile = ContextInitializer.AUTOCONFIG_FILE

  /** Enable logging internal state of Logback by setting `context` and do
    * extra init process.
    *
    * @param context
    */
  override def setContext(context: Context): Unit = {
    super.setContext(context)
    def showExtraParams(where: String, e: Set[String]): Unit = {
        e.foreach(s => addError(s"Logger named $s is defined at $where but it is not used."))
    }
    LoggerManager.validate(loggerSet) match {
      case (l, r) =>
        showExtraParams(LoggerManager.toString, l)
        showExtraParams(ConfigFile, r)
        if (l.nonEmpty || r.nonEmpty) {
          val isNonVerbose = startEvent("[configuration]", saxEvents).exists {
            "false" == _.attributes.getValue("debug")
          }
          System.err.println(
            s"Try to shutdown JVM, because unused logger detected. ${if (isNonVerbose)
               "Enable [configuration][debug] in conf file of Logback to watch more detailed info."
              else
              ""
            }"
            )
          sys.exit(-1)
        }
    }
  }

  /** Get [[StartEvent]] from logback configuration file by `elementPath`.
    *
    * @param elementPath element path to be get
    * @param saxEvents config file represented in format of SAX event
    * @return
    */
  private def startEvent(elementPath: String, saxEvents: Seq[SaxEvent]): Option[StartEvent] = {
    val cand = for {
      se <- saxEvents if se.isInstanceOf[StartEvent]
      see = se.asInstanceOf[StartEvent]
      path = see.elementPath.toString
      if path == elementPath
    } yield {
      see
    }
    if (cand.size > 1) throw new IllegalStateException(s"Duplicated name of element path $elementPath was found as $cand.")
    cand.headOption
  }

  /** Create set of logger names from object representing configuration file.
    *
    * @param saxEvents object representing configuration file
    * @return
    */
  private def createLoggerSet(saxEvents: Seq[SaxEvent]): Set[String] = {
    val m = mutable.Set.empty[String]
    for { sa <- saxEvents } {
      sa match {
        case se: StartEvent =>
          addInfo(s"${se.elementPath} at (line,colmun)=(${se.locator.getLineNumber},${se.locator.getColumnNumber})")
          if (se.localName == "logger") {
            m += se.attributes.getValue("name")
          }
        case _ =>
      }
    }
    m.toSet
  }

  private def logbackUrl: URL = {
    val myClassLoader = Loader.getClassLoaderOfObject(this)
    val url = Loader.getResource(ConfigFile, myClassLoader)
    addInfo(s"loback.xml was found at $url")
    url
  }

  /** Parse and `URL` and return it as SAX object.
    *
    * @param url url of logback configuration file such as `logback.xml`
    * @return
    */
  private def parseXml(url: URL): Seq[SaxEvent] = {
    val urlCon = url.openConnection
    val saxIn = new org.xml.sax.InputSource(urlCon.getInputStream)

    val spf = javax.xml.parsers.SAXParserFactory.newInstance
    spf.setValidating(false)
    spf.setNamespaceAware(true)
    val saxParser = spf.newSAXParser

    val hdr2 = new SaxEventRecorder(context)
    saxParser.parse(saxIn, hdr2)
    hdr2.getSaxEventList.asScala
  }

  override def isResetResistant: Boolean = false

  override def onReset(context: LoggerContext): Unit = { /** no-op */ addInfo("onReset")}

  override def onStart(context: LoggerContext): Unit = { /** no-op */ addInfo("onStart")}

  override def onLevelChange(logger: LBLogger, level: Level): Unit = { /** no-op */ addInfo("onLevelChange")}

  override def onStop(context: LoggerContext): Unit = { /** no-op */ addInfo("onStop")}

}
