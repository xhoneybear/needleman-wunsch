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
        "quiet" -> false,
        "plot" -> true
    )

    /**
      * Extracts sequences from command-line argument map.
      *
      * @param map Command-line argument map
      * @return List of sequences
      */
    private def sequenceList(map: Map[String, Any]): List[(String, String)] =
        map.get("sequences").get.asInstanceOf[List[(String, String)]]

    /**
      * Reads a sequence from a FASTA file.
      *
      * @param path Path to file
      * @return Title and sequence
      */
    private def fromFASTA(path: String): (String, String) =
        val file = fromFile(path).getLines.toStream
        val title = file.filter(_.startsWith(">")).mkString.replace(">", "")
        val seq = file.filterNot(_.startsWith(">")).mkString
        (title, seq)

    /**
      * Prints a help message.
      *
      * @param option Error code, faulty argument or sequence count
      */
    private def printUsage(option: String = "") =
        if (option.startsWith("-"))
            println(s"Unrecognized option: $option")
        else if (option.startsWith("NOT_A_NUMBER"))
            println("Invalid value for an integer argument")
        else if (!option.isEmpty)
            println(s"Got $option sequence arguments, required 2 or more")
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
        println("  -t, --tracklimit VALUE   Maximum number of tracks to plot")
        println("  -n, --nograph            Do not plot a graph")

        System.exit(if (option.toString.isEmpty) 0 else 1)

    /**
      * Recursively process command-line arguments.
      *
      * @param map Command-line argument map
      * @param args Command-line arguments
      * @return Parsed command-line argument map
      */
    private def nextArg(map: Map[String, Any], args: Seq[String]): Map[String, Any] =
        try
            args match
                case Nil => map
                case ("-f" | "--file") :: value :: tail => nextArg(map + ("sequences" -> (sequenceList(map) :+ fromFASTA(value))), tail)
                case ("-g" | "--gap") :: value :: tail => nextArg(map + ("gap" -> value.toInt), tail)
                case ("-m" | "--mismatch") :: value :: tail => nextArg(map + ("mismatch" -> value.toInt), tail)
                case ("-n" | "--nograph") :: tail => nextArg(map + ("plot" -> false), tail)
                case ("-o" | "--output") :: value :: tail => nextArg(map + ("output" -> value), tail)
                case ("-q" | "--quiet") :: tail => nextArg(map + ("quiet" -> true), tail)
                case ("-s" | "--match") :: value :: tail => nextArg(map + ("match" -> value.toInt), tail)
                case ("-t" | "--tracklimit") :: value :: tail => nextArg(map + ("tracklimit" -> value.toInt), tail)
                case value :: _ if value.startsWith("-") => printUsage(value); Map()
                case value :: tail => nextArg(map + ("sequences" -> (sequenceList(map) :+ (value, value))), tail)
        catch
            case _: NumberFormatException => printUsage("NOT_A_NUMBER"); Map()

    /**
      * Parse command-line arguments.
      *
      * @param args Command-line arguments
      * @return Parsed command-line arguments
      */
    def parse(args: Seq[String]): Parameters =
        if (args.contains("-h") || args.contains("--help"))
            printUsage()

        val parsedArgs = Parameters(nextArg(defaultArgs, args))

        if (parsedArgs.sequences.size < 2)
            printUsage(parsedArgs.sequences.size.toString)
        else if (parsedArgs.sequences.size > 2) {
            parsedArgs.maxTracks = 1
            parsedArgs.quiet = true
            parsedArgs.createGraph = false
        }

        parsedArgs
