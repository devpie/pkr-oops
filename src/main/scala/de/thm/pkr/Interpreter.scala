package de.thm.pkr

import scala.collection.mutable.ArrayBuffer

object Storage {
		type Address = Int

		// storage is a heap (open ended list of cells)
		private val heap : ArrayBuffer[Value] = ArrayBuffer();

		// get content of a cell
		def apply(a: Address): Option[Value] = 
			if (a < heap.length) Some(heap(a))
			else None

		// set value at an allocated address
		def assign(a: Address, v: Value) {
			if (a < heap.length) {
				heap(a) = v
			} // else ignore
		}

		// allocate new cell
		def allocateCell(initVal: Value): Address = {
			heap += initVal
			heap.length -1
		} 
}

object Interpreter {
	import Storage.Address
	
	type ValueOrAddress = Either[Value, Address]
	
	// Environment
	// maps names to addresses or values
	case class Env(env: Map[String, ValueOrAddress] = Map()) {
  
		// lookup name
		def apply(x:String): Option[ValueOrAddress] = env.get(x) 

		private def +(name: String, definition: ValueOrAddress): Env =
			Env(env + Pair(name, definition))
		
		// add definition of a definable thing 
		def +(name: String, meaning: Value): Env =
			Env(env + Pair(name, Left(meaning)))
		
		// add definition of an address
		def +(name: String, meaning: Address): Env =
			Env(env + Pair(name, Right(meaning)))
		
	}

	case class inEnv(env: Env) {
		def eval(exp: Exp) : Option[Value] = {
			exp match {
			case IntConst(v) 	=> Some(IntValue(v))
			case BoolConst(v) 	=> Some(BoolValue(v))
			case Var(name)	   	=> {
				val defined = env(name).get
				defined match {
				  case Right(a) => Storage(a) // VAR
				  case Left(v)	=> Some(v)    // CONST
				} 
			}    
			case Greater(e1, e2) 
				=> 	for (v1 <- eval(e1); 
						v2 <- eval(e2);
						v3 <- v1>v2 )
					yield v3

			case Less(e1, e2) 
				=> 	for (v1 <- eval(e1); 
						v2 <- eval(e2);
						v3 <- v1<v2 )
					yield v3

			case Equal(e1, e2) 
				=> 	for (v1 <- eval(e1); 
						v2 <- eval(e2);
						v3 <- v1==v2 )
					yield v3    	    
    
			case Plus(e1, e2) 
				=> 	for (v1 <- eval(e1); 
						v2 <- eval(e2);
						v3 <- v1+v2 )
					yield v3

			case Minus(e1, e2) 
				=> 	for (v1 <- eval(e1); 
						v2 <- eval(e2);
						v3 <- v1-v2)
					yield v3
    	    
			case Times(e1, e2) 
				=> 	for (v1 <- eval(e1); 
						v2 <- eval(e2);
						v3 <- v1*v2)
					yield v3
    	    
			case Div(e1, e2) 
				=> 	for (v1 <- eval(e1); 
						v2 <- eval(e2);
						v3 <- v1/v2)
					yield v3
			}//match
		}// eval
		
		def exec(cmd: Cmd) {
			cmd match {
			case If(e1, cThen, cElse) 
				=> {val v1 = eval(e1)
					if (v1.isDefined && v1.get.isInstanceOf[BoolValue]) {
						if (v1.get.asInstanceOf[BoolValue].b) exec(cThen) else exec(cElse)
					}// else ERROR
				}//If
				
			case Assign(Var(x), e) 
			  	=> {val r = eval(e)
			  	  	val l = env(x)
			  	  	if (l.isDefined) { 
			  	  	  l.get match {
			  	  	    case Right(a) => Storage.assign(a, r.get) //x denotes an address
			  	  	    case Left(a) => None // x denotes non-assignable value
			  	  	  }
			  	  	}
			  	}//Assign
    	 	
		    case While(e:Exp, body:Cmd) 
		    	=> 
		    	  while(true) {
		    		val c = eval(e)
		    		if (c.isDefined && 
		    		    c.get.isInstanceOf[BoolValue] && 
		    		    c.get.asInstanceOf[BoolValue].b) {
		    				exec(body) 
		    		} else {
		    			return
		    		}
		    	}// While
    	

		    case Print(e) 
		    	=> {val v = eval(e)
		    		if (v.isDefined) {
		    		  println(">> "+eval(e).get)
		    		} else  {
		    		  println(">> " + e + " ist undefiniert")
		    		}
		    	  } 
    	
		    case Block(defs, cmds)
		    	=> {val nEnv = define(defs)
		    		cmds.foreach{ inEnv(nEnv).exec(_) }
		    	}
		    	
		    case Call(p, a)
		    	=> {val arg = eval(a)
			  	  	val proc = env(p)
			  	  	if (proc.isDefined && arg.isDefined) { 
			  	  	  proc.get match {
			  	  	    case Right(adr) => None //x denotes an address Stored procedures may not be called
			  	  	    case Left(v) =>  // x denotes non-assignable value
			  	  	    		v match {
			  	  	    		  case procedure: ProcValue => procedure.appy(arg.get, env)
			  	  	    		  case _ => println("ERROR: no procedure");
			  	  	    		}
			  	  	  }
			  	  	}
		    		
		    	}
				
			}// match
		} // exec
		
	  def define(ds: List[Def]) : Env = {
	    var nEnv: Env = env
	    ds foreach { (d) =>
	      d match {
	      	case VarDef(x, initVal)
	      		=> {val v = eval(initVal)
	      			val adr = Storage.allocateCell(v.get)
	      			nEnv = nEnv+(x, adr)
	    	}
	      	case ConstDef(c, initVal)
	      		=> {val v = eval(initVal)
	      		  	nEnv = nEnv+(c, v.get)
	      	}
	      	case ProcDef(p, fp, body)
	      		=> {nEnv = nEnv+(p, ProcValue(fp, body, nEnv))
	      	}
	      }
	    }
	    nEnv
	  }// define
		
	}//inEnv
	
	def interpret(s: String) {
		val ast = CmdParser.parse(s)
		inEnv(Env()) exec ast
	}//interpret

}

