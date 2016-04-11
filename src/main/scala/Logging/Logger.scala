package Logging

import java.text.SimpleDateFormat
import java.util.Calendar

/**
  * Logger.scala
  * This class functions as a program-wide command-line output.
  */
object Logger {
  val format = new SimpleDateFormat("d/M/y H:m:s:S")

  def fatal(message: String, sender: Object): Unit = {
    val c = sender.getClass.getName.replace("$", "")
    val t = format.format(Calendar.getInstance().getTime)

    println(Console.RED + Console.BOLD + "[" + t + "] [" + c + "] FATAL: " + message + Console.RESET)
  }

  def error(message: String, sender: Object): Unit = {
    val c = sender.getClass.getName.replace("$", "")
    val t = format.format(Calendar.getInstance().getTime)

    println(Console.RED + "[" + t + "] [" + c + "] ERROR: " + message + Console.RESET)
  }

  def warn(message: String, sender: Object): Unit = {
    val c = sender.getClass.getName.replace("$", "")
    val t = format.format(Calendar.getInstance().getTime)

    println(Console.YELLOW + "[" + t + "] [" + c + "] WARN: " + message + Console.RESET)
  }

  def info(message: String, sender: Object): Unit = {
    val c = sender.getClass.getName.replace("$", "")
    val t = format.format(Calendar.getInstance().getTime)

    println(Console.GREEN + "[" + t + "] [" + c + "] INFO: " + message + Console.RESET)
  }
}
