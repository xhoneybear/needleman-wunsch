package dev.lisek.needleman_wunsch.util

import dev.lisek.needleman_wunsch.util.Parser.parse

class Parameters(args: Seq[String]):
    val values = parse(args)

    val sequences = values.get("sequences").get.asInstanceOf[List[(String, String)]]
    val matchValue = values.get("match").get.asInstanceOf[Int]
    val gapValue = values.get("gap").get.asInstanceOf[Int]
    val mismatchValue = values.get("mismatch").get.asInstanceOf[Int]
    val maxTracks = values.get("tracklimit").get.asInstanceOf[Int]
    val outputFile = values.get("output").get.asInstanceOf[String]
    val createGraph = values.get("plot").get.asInstanceOf[Boolean]
