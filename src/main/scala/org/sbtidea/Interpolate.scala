package org.sbtidea

object Interpolate {
  private val Variable = """(.*)\$\{(.*?)\}(.*)""".r

  def interpolate(s: String, props: Map[String, String]): String = s match {
    case Variable(prefix, key, suffix) =>
      props.get(key) match {
        case Some(value) => prefix + interpolate(value, props) + suffix
        case None => s
      }
    case _ => s
  }

  def main(args: Array[String]) {
    val props = Map("idea.config.dir" -> "${user.home}/bar", "user.home" -> "/Users/bob")
    assert(interpolate("${idea.config.dir}/plugins", props) == "/Users/bob/bar/plugins")
  }
}