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
    return parseError("noimpl")
  } else if (str[0] == kQuote) {
    return parseError("noimpl")
  }
  return readAtom(str)
}

fun main(args: Array<String>): Unit {
  while(true) {
    print("> ")
    val line = readLine()
    if (line == null) {
      break
    }
    println(read(line).obj)
  }
}
