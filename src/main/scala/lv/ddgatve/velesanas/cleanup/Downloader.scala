package lv.ddgatve.velesanas.cleanup

import scala.io.Source
import scala.xml.XML
import scala.util.matching.Regex

/**
 * uniqueExtractors - regexes and group numbers that represent single fields in the document
 * uniqueFields - names of the single fields
 * tableExtractor - regex to extract the main data table - it matches the whole expression
 * tableFields - all the column names (i.e. names for TDs in their natural order)
 * tidyTable - regexes, how many times to replace and replacement strings.
 *
 */
case class Downloader(individualExtractors: List[(String, Regex, Int)],
    tableExtractor: Regex,
    tableFields: List[String],
    tidyTable: List[(Regex, Int, String)]) {

  /**
   * url - which url to download
   * return value has the unique fields and the table fields
   */
  def extract(url: String): (Map[String, String], List[Map[String, String]]) = {

    val html = Source.fromURL(url)
    val s = html.mkString

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

    //    println(table)

    val myXML = XML.loadString(table)
    val resultTable = for (tableRow <- myXML \\ "TABLE" \\ "TR") yield {
      val fieldValues = (tableRow \\ "TD") map { _.text.trim() }
      (tableFields zip fieldValues).toMap
    }

    (resultMap, resultTable.toList)
  }

}