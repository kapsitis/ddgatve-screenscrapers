package lv.ddgatve.screenscrapers.web

import org.specs2.mutable._
import java.security.MessageDigest

class DownloaderSpec extends Specification {
  "Downloader.getMD5String" should {
    "compute MD5" in {
      val d = new Downloader("/home/kalvis/workspace/cache")
      d.getMD5String("ABC") mustEqual "902FBDD2B1DF0C4F70B4A5D23525E932"
    }
  }

  "Downloader.download" should {
    val url = "http://www.cvk.lv/cgi-bin/wdbcgiw/base/Saeima11.galrez11.kandid?NR1=11"
    val d = new Downloader("/home/kalvis/workspace/cache")
    "download from web" in {
      val s = d.download(url, true)
      s.indexOf("Ģirts Valdis Kristovskis") > 0 mustEqual true
    }
    "download from cache" in {
      val s = d.download(url, false)
      s.indexOf("Ģirts Valdis Kristovskis") > 0 mustEqual true
    }
  }
}