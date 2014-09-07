package lv.ddgatve.velesanas.cleanup

/**
 * Build a set of standardized tables from
 */
class Projection(val dsetNames: List[String],
    val fieldNames: Map[String, List[String]]) {

  private def transformMap(arg: Map[String, String], fields: List[String],
    transforms: Map[String, String]): Map[String, String] = {
    val result = fields map (
      fieldName =>
        {
          if (arg.keySet contains fieldName) {
            (fieldName -> arg.get(fieldName).get)
          } else if (transforms.keySet.contains(fieldName)) {
            (fieldName -> transforms.get(fieldName).get)
          } else {
            (fieldName -> "N/A")
          }

        })
    result.toMap
  }

  def standardize(initial: Map[String, List[Map[String, String]]],
    copyIns: Map[String, Map[String, String]]): Map[String, List[Map[String, String]]] = {
    var result: Map[String, List[Map[String, String]]] = Map()
    for (dsetName <- dsetNames) {
      val inputTable = initial.get(dsetName).get
      val outputTable = inputTable map (x =>
        transformMap(x, fieldNames(dsetName), copyIns(dsetName)))
      result += (dsetName -> outputTable)
    }
    result
  }
}