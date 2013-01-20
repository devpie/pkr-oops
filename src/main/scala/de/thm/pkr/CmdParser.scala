package de.thm.pkr

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object CmdParser extends StandardTokenParsers {
  
  lexical.reserved += ("VAR", "CONST", "WHILE", "DO", "IF", "THEN", "ELSE", "CALL", "TRUE", "FALSE", "PRINT", "BEGIN", "END", "PROC")
  
  lexical.delimiters += ( "+", "-", "*", "/", "=", "(", ")", "<", ">", ":=", ";", "," )
      
  private def exp: Parser[Exp] 
		  		= chainl1(arithExp, arithExp, boolOp)
		  		
  private def boolOp : Parser[(Exp, Exp) ⇒ Exp]
  				= "<" ^^^ {(x:Exp, y: Exp) => Less(x,y)}  | 
  				  ">" ^^^ {(x:Exp, y: Exp) => Greater(x,y)} |
  				  "=" ^^^ {(x:Exp, y: Exp) => Equal(x,y)}
  
  private def arithExp: Parser[Exp] 
		  		= chainl1(term, term, addOp)
		  		
  private def addOp : Parser[(Exp, Exp) ⇒ Exp] 
		  		= "+" ^^^ {(x:Exp, y: Exp) => Plus(x,y)} |
		  		  "-" ^^^ {(x:Exp, y: Exp) => Minus(x,y)}
		  		
  private def term = chainl1(factor, factor, multOp)
  
  private def multOp : Parser[(Exp, Exp) ⇒ Exp]
  				= "*" ^^^ {(x:Exp, y: Exp) => Times(x,y)}  | 
  				  "/" ^^^ {(x:Exp, y: Exp) => Div(x,y)}
  				
  private def factor: Parser[Exp] 
		  		= ident			^^ { name => Var(name) } |
		  		  numericLit   	^^ { d => IntConst(d.toInt) } |
		  		  "TRUE"		^^ { d => BoolConst(true) } |
		  		  "FALSE"		^^ { d => BoolConst(false) } |
  				  "("~>exp<~")"
  				  

  private def cmd: Parser[Cmd]
  				= ("BEGIN"~>defs)~cmds<~"END"^^ { case ds~cs => Block(ds, cs) } | 
		  		  ("IF"~>exp<~"THEN")~cmd~("ELSE"~>cmd) ^^ { case e1~cthen~cElse => If(e1, cthen, cElse) } |
		  		  ("WHILE"~>exp<~"DO")~cmd 				^^ { case e~body => While(e, body) } |
		  		  "PRINT"~>"("~>exp<~")"<~";"			^^ { case e => Print(e) } |
		  		  ("CALL"~>ident)~("("~>exp<~")"<~";")	^^ { case id~e => Call(id, e) } |
		  		  (ident<~":=")~exp<~";"				^^ { case id~e => Assign(Var(id), e)}
  				  
  private def cmds: Parser[List[Cmd]]
		  		= rep(cmd)
		  		  
  private def defs: Parser[List[Def]]
		  		= rep(aDef)
		  		
  private def aDef: Parser[Def]
  				= ("VAR"~>ident)~("="~>exp<~",") ^^ { case name~e => VarDef(name, e) } |
  				  ("CONST"~>ident)~("="~>exp<~",") ^^ { case name~e => ConstDef(name, e) } |
  				  ("PROC"~>ident)~("("~>ident<~")")~cmd<~"," ^^ { case name~fp~c => ProcDef(name, fp, c) }
  
  def parse(s: String) : Cmd = {
    val lexer = new lexical.Scanner(s)
    val result = phrase(cmd)(lexer)
    println(result)
    result match {
        case Success(t,_)     => t
        case NoSuccess(msg,_) => {
        	println(phrase(exp)(lexer))
        	throw new IllegalArgumentException("Parser Error  '" + s + "': " + msg)
        }
  	}
  }
  	
}
