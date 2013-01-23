package de.thm.pkr

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.io.Reader
import scala.util.parsing.input.StreamReader

object CmdParser extends StandardTokenParsers {

  lexical.reserved += ("VAR", "CONST", "WHILE", "DO", "IF", "THEN", "ELSE", "CALL", "TRUE", "FALSE", "PRINT", "BEGIN", "END", "PROC")

  lexical.delimiters += ("+", "-", "*", "/", "=", "(", ")", "<", ">", ":=", ";", ",")

  private def <|>|= =
    "<" ^^^ { Less(_, _) } |
      ">" ^^^ { Greater(_, _) } |
      "=" ^^^ { Equal(_, _) }

  private def +|- = "+" ^^^ { Plus(_, _) } | "-" ^^^ { Minus(_, _) }

  private def *|/ = "*" ^^^ { Times(_, _) } | "/" ^^^ { Div(_, _) }
  
  private def factor =
    ident ^^ { Var(_) } |
      numericLit ^^ { d => IntConst(d.toInt) } |
      "TRUE" ^^ { _ => BoolConst(true) } |
      "FALSE" ^^ { _ => BoolConst(false) } |
      "(" ~> exp <~ ")"

  private def exp: Parser[Exp] = boolExp
  private def boolExp = chainl1(AddDiv, AddDiv, <|>|=)
  private def AddDiv = chainl1(term, term, +|-)
  private def term = chainl1(factor, factor, *|/)

  private def cmd: Parser[Cmd] =
    ("BEGIN" ~> rep(aDef)) ~ rep(cmd) <~ "END" ^^ { case ds ~ cs => Block(ds, cs) } |
      ("IF" ~> exp <~ "THEN") ~ cmd ~ ("ELSE" ~> cmd) ^^ { case e1 ~ cthen ~ cElse => If(e1, cthen, cElse) } |
      ("WHILE" ~> exp <~ "DO") ~ cmd ^^ { case e ~ body => While(e, body) } |
      "PRINT" ~> "(" ~> exp <~ ")" <~ ";" ^^ { case e => Print(e) } |
      ("CALL" ~> ident) ~ ("(" ~> exp <~ ")" <~ ";") ^^ { case id ~ e => Call(id, e) } |
      (ident <~ ":=") ~ exp <~ ";" ^^ { case id ~ e => Assign(Var(id), e) }
  
  private def aDef =
    ("VAR" ~> ident) ~ ("=" ~> exp <~ ",") ^^ { case name ~ e => VarDef(name, e) } |
      ("CONST" ~> ident) ~ ("=" ~> exp <~ ",") ^^ { case name ~ e => ConstDef(name, e) } |
      ("PROC" ~> ident) ~ ("(" ~> ident <~ ")") ~ cmd <~ "," ^^ { case name ~ fp ~ c => ProcDef(name, fp, c) }

  def parse(s: Reader) = {
    val lexer = new lexical.Scanner(StreamReader(s))
    val result = phrase(cmd)(lexer)
    println(result)
    result match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => {
        println(phrase(exp)(lexer))
        throw new IllegalArgumentException("Parser Error  '" + s + "': " + msg)
      }
    }
  }

}
