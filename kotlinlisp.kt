package lisp

val kLPar = '('
val kRPar = ')'
val kQuote = '\''

class Nil {
}
val kNil = Nil()

class Num(n : Number) {
  val data = n
}

class Sym(s : String) {
  val data = s
}
var sym_table : MutableMap<String, Any> = hashMapOf()
fun makeSym(s : String) : Any {
  if (s == "nil") {
    return kNil
  }
  val sym : Any? = sym_table.get(s)
  if (sym != null) {
    return sym
  }
  val new_sym = Sym(s)
  sym_table.put(s, new_sym)
  return new_sym
}
val sym_t = makeSym("t")
val sym_quote = makeSym("quote")
val sym_if = makeSym("if")

class Error(s : String) {
  val data = s
}

class Cons(a : Any, d : Any) {
  var car = a
  var cdr = d
}

class Subr(f : (Any) -> Any) {
  val fn = f
}

class Expr(a : Any, b : Any, e : Any) {
  val args = a
  val body = b
  val env = e
}

fun safeCar(obj : Any) : Any {
  return when (obj) {
    is Cons -> obj.car
    else -> kNil
  }
}

fun safeCdr(obj : Any) : Any {
  return when (obj) {
    is Cons -> obj.cdr
    else -> kNil
  }
}

fun nreverse(l : Any) : Any {
  var lst = l
  var ret : Any = kNil
  while (true) {
    val elm = lst
    when (elm) {
      is Cons -> {
        val tmp = elm.cdr
        elm.cdr = ret
        ret = lst
        lst = tmp
      }
      else -> break
    }
  }
  return ret
}

fun makeExpr(a : Any, e : Any) : Any = Expr(safeCar(a), safeCdr(a), e)

fun isSpace(c : Char) = c == '\t' || c == '\r' || c == '\n' || c == ' '

fun isDelimiter(c: Char) = c == kLPar || c == kRPar || c == kQuote || isSpace(c)

fun skipSpaces(str : String) : String {
  for (i in 0..str.length-1) {
    if (!isSpace(str[i])) {
      return str.substring(i)
    }
  }
  return ""
}

fun makeNumOrSym(str : String) : Any {
  try {
    return Num(str.toInt())
  } catch (e : java.lang.NumberFormatException) {
    return makeSym(str)
  }
}

class ParseState(o : Any, s : String) {
  val obj = o
  val next = s
}

fun readAtom(s : String) : ParseState {
  var str = s
  var next = ""
  for (i in 0..str.length-1) {
    if (isDelimiter(str[i])) {
      next = str.substring(i)
      str = str.substring(0, i)
      break
    }
  }
  return ParseState(makeNumOrSym(str), next)
}

fun parseError(s : String) = ParseState(Error(s), "")

fun read(s : String) : ParseState {
  val str = skipSpaces(s)
  if (str == "") {
    return parseError("empty input")
  } else if (str[0] == kRPar) {
    return parseError("invalid syntax: " + str);
  } else if (str[0] == kLPar) {
    return readList(str.substring(1))
  } else if (str[0] == kQuote) {
    val tmp = read(str.substring(1))
    return ParseState(Cons(sym_quote, Cons(tmp.obj, kNil)), tmp.next)
  }
  return readAtom(str)
}

fun readList(s : String) : ParseState {
  var str = s
  var ret : Any = kNil
  while (true) {
    str = skipSpaces(str)
    if (str == "") {
      parseError("unfinished parenthesis")
    } else if (str[0] == kRPar) {
      break
    }
    val tmp = read(str)
    if (tmp.obj is Error) {
      return tmp
    }
    ret = Cons(tmp.obj, ret)
    str = tmp.next
  }
  return ParseState(nreverse(ret), str.substring(1))
}

fun printObj(obj : Any) : String {
  return when (obj) {
    is Nil -> "nil"
    is Num -> obj.data.toString()
    is Sym -> obj.data
    is Error -> "<error: " + obj.data + ">"
    is Cons -> printList(obj)
    is Subr -> "<subr>"
    is Expr -> "<expr>"
    else -> "<unknown>"
  }
}

fun printList(o : Any) : String {
  var obj = o
  var ret = ""
  var first = true
  while (true) {
    val elm = obj
    when (elm) {
      is Cons -> {
        if (first) {
          first = false
        } else {
          ret += " "
        }
        ret += printObj(elm.car)
        obj = elm.cdr
      }
      else -> break
    }
  }
  if (obj is Nil) {
    return "(" + ret + ")"
  }
  return "(" + ret + " . " + printObj(obj) + ")"
}

fun findVar(sym : Any, e : Any) : Any {
  var env = e
  while (true) {
    val ev = env
    when (ev) {
      is Cons -> {
        var alist = ev.car
        while (true) {
          val al = alist
          when (al) {
            is Cons -> {
              if (safeCar(al.car) == sym) {
                return al.car
              }
              alist = al.cdr
            }
            else -> break
          }
        }
        env = ev.cdr
      }
      else -> break
    }
  }
  return kNil
}

var g_env = Cons(kNil, kNil)

fun addToEnv(sym : Any, value : Any, env : Any) {
  if (env is Cons) {
    env.car = Cons(Cons(sym, value), env.car)
  }
}

fun eval(obj : Any, env : Any) : Any {
  if (obj is Nil || obj is Num || obj is Error) {
    return obj
  } else if (obj is Sym) {
    val bind = findVar(obj, env)
    if (bind is Cons) {
      return bind.cdr
    }
    return Error(obj.data + " has no value")
  }

  val op = safeCar(obj)
  val args = safeCdr(obj)
  if (op == sym_quote) {
    return safeCar(args)
  } else if (op == sym_if) {
    val c = eval(safeCar(args), env)
    if (c is Error) {
      return c
    } else if (c == kNil) {
      return eval(safeCar(safeCdr(safeCdr(args))), env)
    }
    return eval(safeCar(safeCdr(args)), env)
  }
  return apply(eval(op, env), evlis(args, env))
}

fun evlis(l : Any, env : Any) : Any {
  var lst = l
  var ret : Any = kNil
  while (true) {
    val elm = lst
    when (elm) {
      is Cons -> {
        val obj = eval(elm.car, env)
        if (obj is Error) {
          return obj
        }
        ret = Cons(obj, ret)
        lst = elm.cdr
      }
      else -> break
    }
  }
  return nreverse(ret)
}

fun apply(fn : Any, args : Any) : Any {
  if (fn is Error) {
    return fn
  } else if (args is Error) {
    return args
  } else if (fn is Subr) {
    return fn.fn(args)
  }
  return Error(printObj(fn) + " is not function")
}

val subrCar = {(args : Any) : Any ->
  safeCar(safeCar(args))
}

val subrCdr = {(args : Any) : Any ->
  safeCdr(safeCar(args))
}

val subrCons = {(args : Any) : Any ->
  Cons(safeCar(args), safeCar(safeCdr(args)))
}

fun main(args: Array<String>): Unit {
  addToEnv(makeSym("car"), Subr(subrCar), g_env)
  addToEnv(makeSym("cdr"), Subr(subrCdr), g_env)
  addToEnv(makeSym("cons"), Subr(subrCons), g_env)
  addToEnv(sym_t, sym_t, g_env)
  while(true) {
    print("> ")
    val line = readLine()
    if (line == null) {
      break
    }
    println(printObj(eval(read(line).obj, g_env)))
  }
}
