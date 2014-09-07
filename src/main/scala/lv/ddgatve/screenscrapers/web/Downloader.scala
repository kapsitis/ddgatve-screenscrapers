package lv.ddgatve.screenscrapers.web

import scala.io.Source
import scala.xml.XML
import scala.util.matching.Regex
import java.security.MessageDigest
import java.io.File
import org.apache.commons.io.FileUtils

/**
 * uniqueExtractors - names, their regexes and group numbers that represent single fields in the document
 * tableExtractor - regex to extract the main data table - it matches the whole expression
 * tableFields - all the column names (i.e. names for TDs in their natural order)
 * tidyTable - regexes, how many times to replace and replacement strings.
 *
 */
case class Downloader(cachePath: String) {

  def getMD5String(arg: String): String = {
    val hex = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F')
    val bytes = MessageDigest.getInstance("MD5").digest(arg.getBytes)
    val posBytes = bytes map (x => if (x < 0) x + 256 else x)
    val result = bytes map (x => List(hex((x & 240) / 16), hex(x & 15)))
    new String(result.flatten)
  }

  /**
   * url - which url to download
   * return value is the full string stored at the URL
   */
  def download(url: String, fromWeb: Boolean): String = {
    if (fromWeb) {
      println("Downloader.download url=" + url)
      Thread.sleep(1000)
      val html = Source.fromURL(url)
      val s = html.mkString
      val fName = getMD5String(url) + ".html"
      FileUtils.writeStringToFile(new File(cachePath, fName), s, "UTF-8")
      s
    } else {
      val file = new File(cachePath, getMD5String(url) + ".html")
      FileUtils.readFileToString(file)
    }
  }

  def download(url: String): String = {
    val file = new File(cachePath, getMD5String(url) + ".html")
    download(url, !file.exists())
  }

}