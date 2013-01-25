package de.thm.pkr

import java.io.Reader
import scala.util.parsing.input.StreamReader
import scala.util.parsing.combinator.JavaTokenParsers

object CmdParser extends JavaTokenParsers {

  private def +- = """\+|-""".r ^^ { op => (x: Exp, y: Exp) => OP(x, op, y) }
  private def */ = """\*|/""".r ^^ { op => (x: Exp, y: Exp) => OP(x, op, y) }
  private def =<> = """=|<|>""".r ^^ { op => (x: Exp, y: Exp) => OP(x, op, y) }

  private def factor =
    ident ^^ { Var(_) } |
      wholeNumber ^^ { d => IntConst(d.toInt) } |
      """TRUE|FALSE""".r ^^ { d => BoolConst(d.toBoolean) } |
      "(" ~> exp <~ ")"

  private def exp: Parser[Exp] = chainl1(arithExp, arithExp, =<>)

  private def arithExp = chainl1(term, term, +-)

  private def term = chainl1(factor, factor, */)

  private def cmd: Parser[Cmd] = ("BEGIN" ~> rep(aDef)) ~ rep(cmd) <~ "END" ^^ { case ds ~ cs => Block(ds, cs) } |
    ("IF" ~> exp <~ "THEN") ~ cmd ~ ("ELSE" ~> cmd) ^^ { case e1 ~ cthen ~ cElse => If(e1, cthen, cElse) } |
    ("WHILE" ~> exp <~ "DO") ~ cmd ^^ { case e ~ body => While(e, body) } |
    "PRINT" ~> "(" ~> exp <~ ")" <~ ";" ^^ { case e => Print(e) } |
    ("CALL" ~> ident) ~ ("(" ~> exp <~ ")" <~ ";") ^^ { case id ~ e => Call(id, e) } |
    (ident <~ ":=") ~ exp <~ ";" ^^ { case id ~ e => Assign(Var(id), e) }

  private def aDef: Parser[Def] = ("VAR" ~> ident) ~ ("=" ~> exp <~ ",") ^^ { case name ~ e => VarDef(name, e) } |
    ("CONST" ~> ident) ~ ("=" ~> exp <~ ",") ^^ { case name ~ e => ConstDef(name, e) } |
    ("PROC" ~> ident) ~ ("(" ~> ident <~ ")") ~ cmd <~ "," ^^ { case name ~ fp ~ c => ProcDef(name, fp, c) }

  def parse(s: Reader): Cmd = {
    val result = phrase(cmd)(StreamReader(s))
    println(result)
    result match {
      case Success(t, _) => t
      case NoSuccess(msg, _) => {
        throw new IllegalArgumentException("Parser Error  '" + s + "': " + msg)
      }
    }
  }

}
