import util.parsing.combinator._
import util.parsing.input.CharSequenceReader
import io.Source

object BrainScriptParser extends RegexParsers {

	type Type = String
	trait AST
	case class SymbolAST(sym: String) extends AST
	case class NumberAST(num: Int) extends AST
	case class BinOpAST(lhs: AST, op: String, rhs: AST) extends AST
	case class LetAST(ty: Type, sym: String, rhs: AST) extends AST
	case class PrintAST(e: AST) extends AST

	lazy val symbol: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
	lazy val number: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

	lazy val elem: Parser[AST] =
			symbol ^^ { SymbolAST(_) } |
			number ^^ { NumberAST(_) }

	lazy val parseType: Parser[String] = symbol

	lazy val parseExpr: Parser[AST] =
			elem ~ "+" ~ parseExpr ^^ {
				case a~b~c => BinOpAST(a, b, c)
			} |
			elem

	lazy val parseLet: Parser[AST] = parseType ~ symbol ~ "=" ~ parseExpr ^^ {
		case t~a~_~b => LetAST(t, a, b)
	}

	lazy val parsePrint: Parser[AST] = "print" ~ "(" ~ parseExpr ~ ")" ^^ {
		case _~_~a~_ => PrintAST(a)
	}

	lazy val parseStmt: Parser[AST] = (parseLet | parsePrint) <~ ";"

}

object Converter {
	import BrainScriptParser._

	def times(s: String, n: Int) = ("" /: (0 until n)) ((x, _) => x+s)

	var varMap: Map[String, Int] = Map()
	var index: Int = 0

	def convInt(n: Int) = times("+", n)

	def getIndex(s: String): Int = varMap(s)

	def addLocal(s: String) {
		val i = varMap.size
		println("local " + s + ": " + i)
		varMap += s -> i
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

	val plusFunc = "<[-<+>]"

	val zero = "[-]"

	def shift(n: Int) = shiftTo(index + n)

	def load(n: Int) = {
		val s = index - n
		val ls = times("<", s)
		val rs = times(">", s)
		index += 1

		zero + ">" + zero + "<" +  // clear [0, 1]
		ls + "[-" + rs + "+>+<" + ls + "]" + rs +  // copy [n] to [0, 1], [n]=0
		">" + "[-<" + ls + "+>" + rs + "]"  // restore [n] = [1], [1] = 0
	}

	def conv(ast: AST): String = ast match {
		case NumberAST(n) => zero + convInt(n) + shift(1)
		case SymbolAST(s) => load(getIndex(s))
		case BinOpAST(l, "+", r) => conv(l) + conv(r) + { index -= 1; plusFunc }
		case LetAST(t, s, e) => addLocal(s); conv(e)
		case PrintAST(e) => conv(e) + shift(-1) + "."
	}

	var res = ""

	def exec(ast: AST) {
		println(ast)
		res += conv(ast) + " "
	}

	def main(args: Array[String]) {
		var r: Input = new CharSequenceReader(Source.fromFile("test.bs").mkString)
		while({
			parseStmt(r) match {
				case Success(ast, r2) => exec(ast); r = r2; true
				case _ => false
			}
		}) ()
		println(res)
		BrainMa2.eval(res)
		println()
	}

}

