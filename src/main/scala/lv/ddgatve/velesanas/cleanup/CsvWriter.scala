package lv.ddgatve.velesanas.cleanup

import java.io.PrintWriter

object CsvWriter {

  val Pattern = """(\S+)""".r

  def concatenate(strings: List[String]) = strings map { x =>
    x match {
      case Pattern(c) => x
      case _ => "\"" + x + "\""
    }

  } mkString ","

  def write(fname: String, columns: List[String], values: List[List[String]]): Unit = {
    Some(new PrintWriter(fname)).
      foreach {
        p =>
          {
            p.write(concatenate(columns))
            p.write(System.getProperty("line.separator"))
            for (row <- values) {
              p.write(concatenate(row))
              p.write(System.getProperty("line.separator"))
            }
          }
          p.close
      }
  }

}