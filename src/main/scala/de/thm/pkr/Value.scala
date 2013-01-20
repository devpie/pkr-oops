package de.thm.pkr

// values of object language
sealed class Value {
  def + (v:Value): Option[Value] = None;
  def - (v:Value): Option[Value] = None;
  def * (v:Value): Option[Value] = None;
  def / (v:Value): Option[Value] = None;
  def < (v:Value): Option[Value] = None;
  def > (v:Value): Option[Value] = None;
  def == (v:Value): Option[Value] = None;
  def appy (v:Value, callEnv: Interpreter.Env): Unit = None
}

case class IntValue(n:Int) extends Value {

  override def > (v: Value): Option[Value] = v match {
    case IntValue(nn) ⇒ Some(BoolValue(n == nn))
    case _ ⇒ None
  }
  override def < (v: Value): Option[Value] = v match {
    case IntValue(nn) ⇒ Some(BoolValue(n < nn))
    case _ ⇒ None
  } 
  override def == (v: Value): Option[Value] = v match {
    case IntValue(nn) ⇒ Some(BoolValue(n == nn))
    case _ ⇒ None
  } 
  override def + (v: Value): Option[Value] = v match {
    case IntValue(nn) ⇒ Some(IntValue(n + nn))
    case _ ⇒ None
  }
  override def - (v: Value): Option[Value] = v match {
    case IntValue(nn) ⇒ Some(IntValue(n - nn))
    case _ ⇒ None
  }
  override def * (v: Value): Option[Value] = v match {
    case IntValue(nn) ⇒ Some(IntValue(n * nn))
    case _ ⇒ None
  }
  override def / (v: Value): Option[Value] = v match {
    case IntValue(nn) ⇒ Some(IntValue(n / nn))
    case _ ⇒ None
  }
}

case class BoolValue(b: Boolean) extends Value {
  override def == (v:Value): Option[Value] = v match {
    case BoolValue(bb) ⇒ Some(BoolValue(b == bb))
    case _ ⇒ None
  }
}

case class ProcValue(fp: String, body: Cmd, defEnv: Interpreter.Env) extends Value {
  override def appy (v:Value, callEnv: Interpreter.Env): Unit = {
    val exec_env: Interpreter.Env = defEnv+(fp, v)
    Interpreter.inEnv(exec_env) exec body
  }
}
