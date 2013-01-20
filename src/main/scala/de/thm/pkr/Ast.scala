package de.thm.pkr

// abstract Syntax Tree
sealed abstract class Exp
case class IntConst(n: Int) extends Exp
case class BoolConst(b: Boolean) extends Exp
case class Var(x: String) extends Exp
case class Less(l: Exp, r: Exp) extends Exp
case class Greater(l: Exp, r: Exp) extends Exp
case class Equal(l: Exp, r: Exp) extends Exp
case class Plus(l: Exp, r: Exp) extends Exp
case class Minus(l: Exp, r: Exp) extends Exp
case class Times(l: Exp, r: Exp) extends Exp
case class Div(l: Exp, r: Exp) extends Exp

sealed abstract class Cmd
case class Assign(left: Var, right: Exp) extends Cmd
case class While(e:Exp, body:Cmd) extends Cmd
case class If(e: Exp, thenCmd: Cmd, eleseCmd: Cmd) extends Cmd
case class Block(defs: List[Def], cmd: List[Cmd]) extends Cmd
case class Print(e:Exp) extends Cmd
case class Call(proc: String, arg:Exp) extends Cmd

sealed abstract class Def
case class VarDef(name: String, initVal: Exp) extends Def
case class ConstDef(name: String, initVal: Exp) extends Def
case class ProcDef(name: String, param: String, body: Cmd) extends Def