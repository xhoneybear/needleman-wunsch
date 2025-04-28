package dev.lisek.needleman_wunsch.util

import scala.io.Source.fromFile

object Parser:
    val defaultArgs = Map(
        "sequences" -> List[(String, String)](),
        "match" -> 1,
        "gap" -> -1,
        "mismatch" -> -1,
        "output" -> "",
        "tracklimit" -> Int.MaxValue,
        "plot" -> true
    )

    private def sequenceList(map: Map[String, Any]): List[(String, String)] = map.get("sequences").get.asInstanceOf[List[(String, String)]]

    private def fromFASTA(path: String): (String, String) =
        val file = fromFile(path).getLines.toStream
        val title = file.filter(_.startsWith(">")).mkString.replace(">", "")
        val seq = file.filterNot(_.startsWith(">")).mkString
        (title, seq)

    private def printUsage(option: String | Int = "") =
        if (option != 2)
            println(s"Got $option sequence arguments, required 2")
        else if (!option.asInstanceOf[String].isEmpty)
            println(s"Unrecognized option: $option")
        else
            println("Needleman-Wunsch algorithm")
            println("Compare two sequences and display their similarity statistics.")

        println("\nUsage: needleman-wunsch [options] [-f] sequence1 [-f] sequence2")
        println("Options:")
        println("  -f, --file FILENAME      Parse from file instead of stdin")
        println("  -o, --output FILE        File to output statistics to")
        println("  -s, --match VALUE        Score for match")
        println("  -m, --mismatch VALUE     Penalty for mismatch")
        println("  -g, --gap VALUE          Penalty for gap")
        println("  -n, --nograph            Do not plot a graph")

        System.exit(if (option.toString.isEmpty) 0 else 1)

    private def nextArg(map: Map[String, Any], args: Seq[String]): Map[String, Any] =
        try
            args match
                case Nil => map
                case ("-f" | "--file") :: value :: tail => nextArg(map + ("sequences" -> (sequenceList(map) :+ fromFASTA(value))), tail)
                case ("-g" | "--gap") :: value :: tail => nextArg(map + ("gap" -> value.toInt), tail)
                case ("-m" | "--mismatch") :: value :: tail => nextArg(map + ("mismatch" -> value.toInt), tail)
                case ("-n" | "--nograph") :: tail => nextArg(map + ("plot" -> false), tail)
                case ("-o" | "--output") :: value :: tail => nextArg(map + ("output" -> value), tail)
                case ("-s" | "--match") :: value :: tail => nextArg(map + ("match" -> value.toInt), tail)
                case ("-t" | "--tracklimit") :: value :: tail => nextArg(map + ("tracklimit" -> value.toInt), tail)
                case value :: tail => nextArg(map + ("sequences" -> (sequenceList(map) :+ (value, value))), tail)
        catch
            case _: NumberFormatException => printUsage("NOT_A_NUMBER"); Map()

    def parse(args: Seq[String]): Map[String, Any] =
        if (args.contains("-h") || args.contains("--help"))
            printUsage()

        nextArg(defaultArgs, args)
