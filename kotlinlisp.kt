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
val sym_quote = makeSym("quote")

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
    is Error -> "<error: " + obj + ">"
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

fun main(args: Array<String>): Unit {
  while(true) {
    print("> ")
    val line = readLine()
    if (line == null) {
      break
    }
    println(printObj(read(line).obj))
  }
}