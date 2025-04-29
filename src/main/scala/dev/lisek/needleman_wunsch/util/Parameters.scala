package dev.lisek.needleman_wunsch.util

import dev.lisek.needleman_wunsch.util.Parser.parse

class Parameters(args: Map[String, Any]):
    val sequences = args.get("sequences").get.asInstanceOf[List[(String, String)]]
    val matchValue = args.get("match").get.asInstanceOf[Int]
    val gapValue = args.get("gap").get.asInstanceOf[Int]
    val mismatchValue = args.get("mismatch").get.asInstanceOf[Int]
    val maxTracks = args.get("tracklimit").get.asInstanceOf[Int]
    val outputFile = args.get("output").get.asInstanceOf[String]
    val createGraph = args.get("plot").get.asInstanceOf[Boolean]
