package de.thm.pkr

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import java.io.Reader

trait Types {
  type OptVal = Option[Value]
  type Address = Int
  type ValueOrAddress = Either[Value, Address]
  val operators =
    Map[String, Function2[Value, Value, OptVal]]("+" -> { _ + _ }, "-" -> { _ - _ }, "*" -> { _ * _ }, "/" -> { _ / _ }, "==" -> { _ == _ }, "<" -> { _ < _ }, ">" -> { _ > _ })

}

object Storage extends Types {

  // storage is a heap (open ended list of cells)
  private val heap = ArrayBuffer[Value]();

  // get content of a cell
  def apply(a: Address) =
    if (a < heap.length) Some(heap(a))
    else None

  // set value at an allocated address
  def assign(a: Address, v: Value) =
    if (a < heap.length) heap(a) = v // else ignore

  // allocate new cell
  def allocateCell(initVal: Value) = {
    heap += initVal
    heap.length - 1
  }
}

object Interpreter extends Types {

  // Environment
  // maps names to addresses or values
  case class Env(env: Map[String, ValueOrAddress] = Map()) {

    // lookup name
    def apply(x: String): Option[ValueOrAddress] = env.get(x)

    private def +(name: String, definition: ValueOrAddress) =
      Env(env + Pair(name, definition))

    // add definition of a definable thing 
    def +(name: String, meaning: Value) = Env(env + Pair(name, Left(meaning)))

    // add definition of an address
    def +(name: String, meaning: Address) = Env(env + Pair(name, Right(meaning)))

  }

  case class inEnv(env: Env) {
    def eval(exp: Exp): OptVal = {
      exp match {
        case IntConst(v) => Some(IntValue(v))
        case BoolConst(v) => Some(BoolValue(v))
        case Var(name) =>
          env(name) flatMap {
            case Right(a) => Storage(a) // VAR
            case Left(v) => Some(v) // CONST
          }
        case OP(e1, op, e2) => eval2(e1, e2, operators(op))
      } //match
    } // eval

    private def eval2(e1: Exp, e2: Exp, op: (Value, Value) => OptVal) = {
      for (
        v1 <- eval(e1);
        v2 <- eval(e2);
        v3 <- op(v1, v2)
      ) yield v3
    }

    def exec(cmd: Cmd) {
      cmd match {
        case If(e1, cThen, cElse) => {
          eval(e1) map { b =>
            if (b.isInstanceOf[BoolValue]) {
              if (b.asInstanceOf[BoolValue].b) exec(cThen) else exec(cElse)
            } // else ERROR
          }
        } //If

        case Assign(Var(x), e) => {
          val r = eval(e)
          env(x) map {
            case Right(a) => Storage.assign(a, r.get) //x denotes an address
            case Left(a) => None // x denotes non-assignable value
          }
        } //Assign

        case While(e: Exp, body: Cmd) =>
          while (true) {
            eval(e) map { b =>
              if (b.isInstanceOf[BoolValue] &&
                b.asInstanceOf[BoolValue].b) {
                exec(body)
              } else {
                return
              }
            }
          } // While

        case Print(e) => {
          println(">> " + { eval(e) getOrElse { e + " ist undefiniert" } })
        }

        case Block(defs, cmds) => {
          val nEnv = define(defs)
          cmds foreach { inEnv(nEnv) exec _ }
        }

        case Call(p, a) =>
          eval(a) map { arg =>
            env(p) map {
              case Right(adr) => () //x denotes an address Stored procedures may not be called
              case Left(procedure: ProcValue) => // x denotes non-assignable value
                procedure.appy(arg, env)
              case _ => println("ERROR: no procedure");
            }
          }

      }
    }

    def define(ds: List[Def]): Env = {
      var nEnv: Env = env
      ds foreach {
        _ match {
          case VarDef(x, initVal) => {
            val v = eval(initVal)
            val adr = Storage.allocateCell(v.get)
            nEnv = nEnv + (x, adr)
          }
          case ConstDef(c, initVal) => {
            val v = eval(initVal)
            nEnv = nEnv + (c, v.get)
          }
          case ProcDef(p, fp, body) => {
            nEnv = nEnv + (p, ProcValue(fp, body, nEnv))
          }
        }
      }
      nEnv
    } // define

  }

  def interpret(s: Reader) { inEnv { Env() } exec CmdParser.parse(s) }

}

