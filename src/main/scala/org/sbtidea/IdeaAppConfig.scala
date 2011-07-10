package org.sbtidea

import java.io.{FileInputStream, File}
import java.util.Properties
import xml._
import sbt.Logger
import scala.util.Properties.isMac

/**
 * Represents the Application level config of IntelliJ, in which the template project settings are found.
 */
final class IdeaAppConfig(val configDir: File) {
  /**
   * Extracts the nodes matching the XPath \\component[name=componentName]
   * from `options/project.default.xml`.
   */
  def defaults(componentName: String): Seq[Node] = {
    def attrib(n: Node, name: String): Option[String] = n match {
      case n: xml.Elem => n.attribute(name).flatMap(_.map(_.text).headOption)
      case _ => None
    }

    (defaultsXml \\ "component").collect {
      case x: xml.Elem if attrib(x, "name") == Some(componentName) => x
    }
  }

  private lazy val defaultsFile: Option[File] = {
    new File(configDir, "options/project.default.xml") match {
      case x if x.exists() => Some(x)
      case _ => None
    }
  }

  private lazy val defaultsXml: xml.Node = defaultsFile.map {
    f =>
      val is = new FileInputStream(f)
      try {
        XML.load(is)
      } finally {
        is.close()
      }
  }.getOrElse(<dummy/>)
}

object IdeaAppConfig {

  import System._

  def apply(log: Logger): Option[IdeaAppConfig] = {
    if (isMac) {
      // Should look in Info.plist, but we don't know where the IDEA install dir is.
      val f = new File(getProperty("user.home"), "Library/Preferences/IntelliJIdea10")
      log.debug("Searching for idea.config.dir in %s".format(f.getAbsolutePath))
      if (f.exists) Some(new IdeaAppConfig(f)) else None
    } else {
      locateIdeaPropertiesWindowsLinux(log) match {
        case Some(ideaProperties) =>
          val properties = loadProperties(ideaProperties)
          val configPath = properties("idea.config.path")
          val fullConfigPath: String = Interpolate.interpolate(configPath, properties)
          val configPathFile = new File(fullConfigPath)
          if (configPathFile.exists) {
            Some(new IdeaAppConfig(configPathFile))
          } else None
        case None => None
      }
    }
  }

  // IDEA searches, in order:
  // Windows / Linux(?):
  // 1. Environmment variable IDEA_PROPERTIES
  // 2. ~/.idea.properties
  // 3. ${idea.home}/bin/idea.properties
  //
  // We only search 1 and 2, as we don't know where IDEA is installed.
  private def locateIdeaPropertiesWindowsLinux(log: Logger): Option[File] = {
    val firstTry = {
      log.debug("Searching for idea.properties using IDEA_PROPERTIES environment variable.")
      Option(getenv("IDEA_PROPERTIES")).flatMap(toExistingFile)
    }
    val secondTry = {
      val f = new File(getProperty("user.home"), "idea.properties")
      log.debug("Searching for idea.properties in home directory")
      if (f.exists) Some(f) else None
    }
    firstTry.orElse(secondTry)
  }

  /** Loads the properties in `ideaProperties` and merges with the system properties */
  private def loadProperties(ideaProperties: File): Map[String, String] = {
    val inputStream = new FileInputStream(ideaProperties)
    val properties = new Properties()
    try {
      properties.load(inputStream)
    } finally {
      inputStream.close()
    }

    toMap(properties) ++ toMap(System.getProperties)
  }

  private def toMap(properties: Properties): Map[String, String] = {
    import scala.collection.JavaConversions._

    properties.stringPropertyNames().map(x => (x, properties.get(x).asInstanceOf[String])).toMap
  }


  private def toExistingFile(path: String): Option[File] = {
    val f = new File(path)
    if (f.exists) Some(f) else None
  }
}