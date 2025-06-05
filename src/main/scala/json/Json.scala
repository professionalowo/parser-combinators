package de.professionalowo
package json

type JSON = JsonArray
  | JsonObject
  | JsonString
  | JsonBoolean
  | JsonNull

case class JsonNull() {
  /*override def toString: String = "null"*/
}

case class JsonBoolean(value: Boolean) {
  /*override def toString: String = value.toString*/
}

case class JsonString(value: String) {
  /*override def toString: String = "\"" + value + "\""*/
}

case class JsonArray(items: List[JSON]) {
  /*override def toString: String = "[" + items.mkString(", ") + "]"*/
}

case class JsonObject(fields: Map[JsonString, JSON]) {
  /*override def toString: String = "\"{ "
    + fields.map((name, value) => name.toString + ": " + value.toString)
    .mkString(", ")
    + " }\""*/
}