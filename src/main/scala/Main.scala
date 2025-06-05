package de.professionalowo

import json.JSON

@main def main(args: String*): Unit
= println(JSON.parse(jsonString))

val jsonString =
  """{"field": {"inner": "foobar"}, "field2": "value", "third": null,"array": ["hallo", "hallo", {"in": "array"}, false, []]}"""