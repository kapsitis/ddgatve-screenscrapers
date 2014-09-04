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

  def getIndividualExtractors(): Seq[(String, Regex, Int)] = {
    theProfile \\ "individualExtractors" \\ "item" map
      (x =>
        (x.attribute("name").head.text,
          patternMap.get(x.attribute("patternId").head.text).get,
          x.attribute("group").head.text.toInt))
  }

  def getTableExtractor(): Regex = patternMap.get(
    (theProfile \\ "tableExtractor").head.attribute("patternId").head.text).get

  def getTableColumns(): Seq[String] = (theProfile \\ "tableColumns" \\ "item") map
    (_.attribute("name").head.text)

  def getTidyPatterns(): Seq[(Regex, Int, String)] = (theProfile \\ "tidyPatterns" \\ "item") map
    (x =>
      (patternMap.get(x.attribute("patternId").head.text.trim).get,
        x.attribute("num").head.text.trim.toInt,
        x.head.text.trim))

}