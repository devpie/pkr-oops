package de.thm.pkr

import scala.io.Source

object Main extends App {
	args foreach {
	  Interpreter interpret Source.fromFile(_).reader
	}
}