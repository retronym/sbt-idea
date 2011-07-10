package org.sbtidea

/**
 * Copyright (C) 2010, Mikko Peltonen, Ismael Juma, Jon-Anders Teigen, Jason Zaugg
 * Licensed under the new BSD License.
 * See the LICENSE file for details.
 */

import sbt._
import xml.transform.{RewriteRule, RuleTransformer}
import java.nio.channels.Channels
import util.control.Exception._
import xml.{Text, Elem, XML, Node}
import java.io.{FileOutputStream, File}

object OutputUtil {
  def saveFile(dir: File, filename: String, node: xml.Node) { saveFile(new File(dir, filename), node) }

  def saveFile(file: File, node: xml.Node) {
    val prettyPrint = new scala.xml.PrettyPrinter(150, 2)
    val fos = new FileOutputStream(file)
    val w = Channels.newWriter(fos.getChannel(), XML.encoding)

    ultimately(w.close())(
      w.write(prettyPrint.format(node))
    )
  }
}

class IdeaProjectDescriptor(val projectInfo: IdeaProjectInfo, val env: IdeaProjectEnvironment, val log: Logger) {

  def projectRelative(file: File) = {
    IO.relativize(projectInfo.baseDir, file.getCanonicalFile).map ("$PROJECT_DIR$/" + _).getOrElse(file.getCanonicalPath)
  }

  val vcsName = List("svn", "Git").foldLeft("") { (res, vcs) =>
    if (new File(projectInfo.baseDir, "." + vcs.toLowerCase).exists) vcs else res
  }

  private def moduleEntry(pathPrefix: String, moduleName: String, groupName: Option[String]) =
    <module fileurl={String.format("file://$PROJECT_DIR$%s/%s.iml", pathPrefix, moduleName)}
            filepath={String.format("$PROJECT_DIR$%s/%s.iml", pathPrefix, moduleName)}
            group={groupName map xml.Text} />

  private def projectModuleManagerComponent: xml.Node =
    <component name="ProjectModuleManager">
      <modules>
      {
        env.includeSbtProjectDefinitionModule match {
          case true => moduleEntry("/" + env.modulePath.get, "project", None)
          case _ =>
        }
      }
      {
        for {moduleInfo <- projectInfo.childProjects
             pathPrefix = if (env.modulePath.isDefined) "/" + env.modulePath.get else moduleInfo.baseDir.getCanonicalPath} yield {
          moduleEntry(pathPrefix, moduleInfo.name, moduleInfo.ideaGroup)
        }
      }
      </modules>
    </component>

  private def project(inner: Seq[xml.Node]): xml.Node = <project version="4">{inner}</project>

  private def libraryTableComponent(library: IdeaLibrary): xml.Node =
    <component name="libraryTable">
      <library name={library.name}>
        <CLASSES>
          {
          library.classes.map(file => {
            <root url={String.format("jar://%s!/", projectRelative(file))} />
          })
          }
        </CLASSES>
        <JAVADOC>
        {
        library.javaDocs.map(file => <root url={String.format("jar://%s!/", projectRelative(file))} />)
        }
        </JAVADOC>
        <SOURCES>
          {
          library.sources.map(file => {
            <root url={String.format("jar://%s!/", projectRelative(file))} />
          })
          }
        </SOURCES>
      </library>
    </component>

  private def projectRootManagerComponent: xml.Node =
      <component name="ProjectRootManager" version="2" languageLevel={env.javaLanguageLevel}
                 assert-keyword="true" jdk-15="true" project-jdk-name={env.projectJdkName} project-jdk-type="JavaSDK" />

  private def projectDetailsComponent: xml.Node =
    <component name="ProjectDetails">
      <option name="projectName" value={projectInfo.name} />
    </component>

  private def vcsComponent: xml.Node =
    <component name="VcsDirectoryMappings">
      <mapping directory="" vcs={vcsName} />
    </component>

  def save() {
    import OutputUtil.saveFile

    if (projectInfo.baseDir.exists) {
      val configDir = new File(projectInfo.baseDir, ".idea")
      def configFile(name: String) = new File(configDir, name)
      configDir.mkdirs

      Seq(
        "modules.xml" -> Some(project(Seq(projectModuleManagerComponent))),
        "misc.xml" -> miscXml(configDir).map(miscTransformer.transform).map(_.head)
      ) foreach {
        case (fileName, Some(xmlNode)) => saveFile(configDir, fileName, xmlNode)
        case _ =>
      }


      if (!configFile("vcs.xml").exists) {
        val defaultVcsComponentNames = Seq("IssueNavigationConfiguration")
        saveFile(configDir, "vcs.xml", project(defaultComponents(defaultVcsComponentNames) ++ vcsComponent))
      }

      val librariesDir = configFile("libraries")
      librariesDir.mkdirs
      for (ideaLib <- projectInfo.ideaLibs) {
        // MUST all be _
        val filename = ideaLib.name.replace('.', '_').replace('-', '_') + ".xml"
        saveFile(librariesDir, filename, libraryTableComponent(ideaLib))
      }

      log.info("Created " + configDir)
    } else log.error("Skipping .idea creation for " + projectInfo.baseDir + " since directory does not exist")
  }

  private lazy val ideaAppConfig: Option[IdeaAppConfig] = {
    try {
      val ac = IdeaAppConfig.apply(log)
      ac match {
        case Some(x) => log.info("Using template project settings from: " + x.configDir.absolutePath)
        case None => log.info("Could not locate IDEA config dir. Template Project Settings will not be used for .idea_project/{misc, vsc}.xml.")
      }
      ac
    } catch {
      case x =>
        log.trace(x)
        log.warn(x.getMessage)
        None
    }
  }

  private def defaultComponents(names: Seq[String]): Seq[Node] = ideaAppConfig match {
    case Some(gc) => names.flatMap(gc.defaults)
    case None => Seq()
  }

  private lazy val defaultMiscXml = {
    val defaultMiscComponentNames = Seq("DependencyValidationManager", "DependenciesAnalyzeManager",
      "JavadocGenerationManager", "ProjectResources", "MavenProjectsManager", "NullableNotNullManager",
      "MavenCompilerTasksManager", "MavenProjectNavigator", "MavenRunner")
    project(defaultComponents(defaultMiscComponentNames) ++ projectRootManagerComponent)
  }

  private def miscXml(configDir: File): Option[Node] = try {
    Some(XML.loadFile(new File(configDir, "misc.xml")))
  } catch {
    case e: java.io.FileNotFoundException => Some(defaultMiscXml)
    case e: org.xml.sax.SAXParseException => {
      log.error("Existing .idea/misc.xml is not well-formed. Reset to default [y/n]?")
      val key = System.console.reader.read
      if (key == 121 /*y*/ || key == 89 /*Y*/ ) Some(defaultMiscXml) else None
    }
  }

  private object miscTransformer extends RuleTransformer(
    new RewriteRule () {
      override def transform (n: Node): Seq[Node] = n match {
        case e @ Elem(_, "component", _, _, _*) if e \ "@name" == Text("ProjectDetails") => projectDetailsComponent
        case e @ Elem(_, "component", _, _, _*) if e \ "@name" == Text("ProjectRootManager") => projectRootManagerComponent
        case _ => n
      }
    }
  )
}
