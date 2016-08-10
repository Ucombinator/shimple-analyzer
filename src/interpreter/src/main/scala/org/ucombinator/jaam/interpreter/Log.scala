package org.ucombinator.jaam.interpreter

import com.esotericsoftware.minlog

// Logging that does not evaluate its message unless it is actually printed
object Log {
  // TODO: make these more efficient by making them macros
  def error(message : => String) : Unit = if (minlog.Log.ERROR) minlog.Log.error(null, message, null)
  def warn(message : => String) : Unit = if (minlog.Log.WARN) minlog.Log.warn(null, message, null)
  def info(message : => String) : Unit = if (minlog.Log.INFO) minlog.Log.info(null, message, null)
  def debug(message : => String) : Unit = if (minlog.Log.DEBUG) minlog.Log.debug(null, message, null)
  def trace(message : => String) : Unit = if (minlog.Log.TRACE) minlog.Log.trace(null, message, null)

  def setLogging(level : String) = {
    level.toLowerCase match {
      case "none" => minlog.Log.set(minlog.Log.LEVEL_NONE)
      case "error" => minlog.Log.set(minlog.Log.LEVEL_ERROR)
      case "warn" => minlog.Log.set(minlog.Log.LEVEL_WARN)
      case "info" => minlog.Log.set(minlog.Log.LEVEL_INFO)
      case "debug" => minlog.Log.set(minlog.Log.LEVEL_DEBUG)
      case "trace" => minlog.Log.set(minlog.Log.LEVEL_TRACE)
    }
  }

  minlog.Log.setLogger(new JaamLogger)

  class JaamLogger extends minlog.Log.Logger {
    private var level = 0

    override def log(level : Int, category : String, message : String, ex : Throwable) {
      this.level = level
      super.log(level, category, message, ex)
    }

    override def print(s : String) = {
      this.level match {
        case minlog.Log.LEVEL_ERROR => super.print(Console.RED + s + Console.RESET)
        case minlog.Log.LEVEL_WARN => super.print(Console.YELLOW + s + Console.RESET)
        case _ => super.print(s)
      }
    }
  }
}
