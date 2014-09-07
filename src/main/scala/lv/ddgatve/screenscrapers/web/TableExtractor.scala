package lv.ddgatve.screenscrapers.web

import scala.io.Source
import scala.xml.XML
import scala.util.matching.Regex
import lv.ddgatve.velesanas.cleanup.ConfigurationReader

case class TableExtractor(confFile: String, profile: String) {
  val cr = new ConfigurationReader(confFile, profile)
  val individualExtractors = cr.getIndividualExtractors
  val tableExtractor = cr.getTableExtractor
  val tidyTable = cr.getTidyPatterns
  val tableFields = cr.getTableColumns

  val downloader = new Downloader("/home/kalvis/workspace/cache")

  def extract(url: String): Map[String, List[Map[String, String]]] = {
    //    println("TableExtractor url=" + url)

    val s = downloader.download(url)
    var resultMap: Map[String, String] = Map()
    for (i <- 0 until individualExtractors.size) {
      val re = individualExtractors(i)._2
      val foundRe = re.findFirstMatchIn(s)
      foundRe match {
        case Some(m) => {
          resultMap += (individualExtractors(i)._1 -> m.group(individualExtractors(i)._3))
        }
        case None => {
          resultMap += (individualExtractors(i)._1 -> "")
          println("ERROR: Could not find unique field " + individualExtractors(i)._1 +
            " in url " + url)
        }
      }
    }

    var table = tableExtractor.findFirstIn(s).get
    for (re <- tidyTable) {
      if (re._2 == -1) {
        table = re._1.replaceAllIn(table, re._3)
      }
      if (re._2 == 1) {
        table = re._1.replaceFirstIn(table, re._3)
      }
    }
    //    println("TABLE IS " + table)

    val myXML = XML.loadString(table)

    val resultTable = for (tableRow <- myXML \\ "TABLE" \\ "TR") yield {
      val fieldValues = (tableRow \\ "TD") map { _.text.trim() }
      (tableFields zip fieldValues).toMap
    }
    Map("singleCols" -> List(resultMap), "candidates" -> resultTable.toList)
  }
}