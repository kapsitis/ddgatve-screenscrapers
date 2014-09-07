package lv.ddgatve.velesanas.cleanup

import scala.xml.XML
import scala.util.matching.Regex
import scala.xml.Node

class ConfigurationReader(f: String, profile: String) {

  val conf = XML.loadFile(f)

  var patternMap: Map[String, Regex] = Map()
  for (p <- conf \\ "profiles" \\ "patterns" \\ "pattern") {
    patternMap += (p.attribute("id").head.text -> p.text.trim.r)
  }

  val theProfile = ((conf \\ "profiles" \\ "profile") filter
    { _.attribute("id").head.text == profile }).head

  val urlPrefix = (theProfile \\ "urlPrefix").head.text.trim

  def getUrlPrefix(): String = urlPrefix

  def getPatternMap(): Map[String, Regex] = patternMap

  def getProfile = theProfile

  def getIndividualExtractors(): List[(String, Regex, Int)] = {
    val result = theProfile \\ "individualExtractors" \\ "item" map
      (x =>
        (x.attribute("name").head.text,
          patternMap.get(x.attribute("patternId").head.text).get,
          x.attribute("group").head.text.toInt))
    result.toList
  }

  def getTableExtractor(): Regex = patternMap.get(
    (theProfile \\ "tableExtractor").head.attribute("patternId").head.text).get

  def getTableColumns(): List[String] = {
    val result = (theProfile \\ "tableColumns" \\ "item") map
      (_.attribute("name").head.text)
    result.toList
  }

  def getTidyPatterns(): List[(Regex, Int, String)] = {
    val result = theProfile \\ "tidyPatterns" \\ "item" map
      (x =>
        (patternMap.get(x.attribute("patternId").head.text.trim).get,
          x.attribute("num").head.text.trim.toInt,
          x.head.text.trim))
    result.toList
  }

}