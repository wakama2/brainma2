import util.parsing.combinator._
import util.parsing.input.CharSequenceReader
import io.Source

object BrainScriptParser extends RegexParsers {
	//---
	type Type = String
	trait AST
	case class SymbolAST(sym: String) extends AST
	case class NumberAST(num: Int) extends AST
	case class CallAST(fname: String, args: Seq[AST]) extends AST
	case class LetAST(ty: Type, sym: String, rhs: AST) extends AST

	//---
	lazy val symbol: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
	lazy val number: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

	//---
	lazy val parseType: Parser[Type] = symbol

	lazy val parseFactor: Parser[AST] =
			"(" ~> parseExpr <~ ")" |
			symbol ^^ { SymbolAST(_) } |
			number ^^ { NumberAST(_) }

	lazy val parseTerm: Parser[AST] =
			parseFactor ~ rep(("*"|"/") ~ parseFactor) ^^ {
				case a~b => (a /: b) { case (a, (op~c)) => CallAST(op, Seq(a, c)) }
			}

	lazy val parseExpr: Parser[AST] =
			parseTerm ~ rep(("+"|"-") ~ parseTerm) ^^ {
				case a~b => (a /: b) { case (a, (op~c)) => CallAST(op, Seq(a, c)) }
			}

	lazy val parseLet: Parser[AST] = parseType ~ symbol ~ "=" ~ parseExpr ^^ {
		case t~a~_~b => LetAST(t, a, b)
	}

	lazy val parsePrint: Parser[AST] = "print" ~ "(" ~ parseExpr ~ ")" ^^ {
		case _~_~a~_ => CallAST("print", Seq(a))
	}

	lazy val parseStmt: Parser[AST] = (parseLet | parsePrint) <~ ";"

}

object BrainScript {
	import BrainScriptParser._

	//def D(msg: Any) = {} // debug
	def D(msg: Any) = println(msg)

	def times(s: String, n: Int) = ("" /: (0 until n)) ((x, _) => x+s)

	var varMap: Map[String, Int] = Map()
	var index: Int = 0

	def convInt(n: Int) = times("+", n)

	def getIndex(s: String): Int = varMap(s)

	def addLocal(s: String) {
		val i = varMap.size
		varMap += s -> i
		D("local " + s + ": " + i)
	}

	def shiftTo(n: Int): String = {
		val index0 = index
		index = n
		if(n > index0) {
			times(">", n - index0)
		} else {
			times("<", index0 - n)
		}
	}

	def shift(n: Int) = shiftTo(index + n)

	val zero = "[-]"

	def load(n: Int) = {
		val s = index - n
		val ls = times("<", s)
		val rs = times(">", s)
		index += 1

		zero + ">" + zero + "<" +  // clear [0, 1]
		ls + "[-" + rs + "+>+<" + ls + "]" + rs +  // copy [n] to [0, 1], [n]=0
		">" + "[-<" + ls + "+>" + rs + "]"  // restore [n] = [1], [1] = 0
	}

	case class Func(src: String, retSize: Int)

	val funcs: Map[String, Func] = Map(
		"+" -> Func("<[-<+>]", 1),
		"-" -> Func("<[-<->]", 1),
		"*" -> Func("[-]>[-]<<<[->>+<<]>[->[->+<<<+>>]>[-<+>]<<]", 1),
		"print" -> Func("<.", 0)
	)

	def convCall(fname: String, args: Seq[AST]) = {
		val func = funcs(fname)
		(("" /: args) (_ + conv(_))) +
			{ index -= args.size - func.retSize; func.src }
	}

	def conv(ast: AST): String = ast match {
		case NumberAST(n) => zero + convInt(n) + shift(1)
		case SymbolAST(s) => load(getIndex(s))
		case LetAST(t, s, e) => addLocal(s); conv(e)
		case CallAST(fname, args) => convCall(fname, args)
	}

	var res = ""

	def exec(ast: AST) {
		D(ast)
		res += conv(ast) + " "
	}

	def main(args: Array[String]) {
		var r: Input = new CharSequenceReader(Source.fromFile(args(0)).mkString)
		while({
			parseStmt(r) match {
				case Success(ast, r2) => exec(ast); r = r2; true
				case _ => false
			}
		}) ()
		D(res)
		BrainMa2.eval(res)
		println()
	}

}

