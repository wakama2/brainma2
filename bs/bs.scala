import util.parsing.combinator._
import util.parsing.input.CharSequenceReader
import io.Source

object BrainScriptParser extends RegexParsers {
	//---
	type Type = String
	trait AST
	case class VarAST(sym: String) extends AST
	case class NumberAST(num: Int) extends AST
	case class CallAST(fname: String, args: Seq[AST]) extends AST
	case class VarStmtAST(ty: Type, sym: String, rhs: AST) extends AST
	case class BlockStmtAST(b: Seq[AST]) extends AST
	case class IfStmtAST(cond: AST, ts: AST, es: Option[AST]) extends AST
	case class WhileStmtAST(cond: AST, b: AST) extends AST

	//---
	lazy val symbol: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
	lazy val number: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

	//---
	lazy val parseType: Parser[Type] = symbol
	lazy val parseElem: Parser[AST] = {
			"(" ~> parseExpr <~ ")" |
			parseCall |
			symbol ^^ { VarAST(_) } |
			number ^^ { NumberAST(_) }
	}
	lazy val parseCall: Parser[AST] = {
		symbol ~ ("(" ~> repsep(parseExpr, ",") <~ ")") ^^ {
			case f~a => CallAST(f,a)
		}
	}
	lazy val parseMulExpr: Parser[AST] = {
			parseElem ~ rep(("*"|"/") ~ parseElem) ^^ {
				case a~b => (a /: b) { case (a, (op~c)) => CallAST(op, Seq(a, c)) }
			}
	}
	lazy val parseAddExpr: Parser[AST] = {
			parseMulExpr ~ rep(("+"|"-") ~ parseMulExpr) ^^ {
				case a~b => (a /: b) { case (a, (op~c)) => CallAST(op, Seq(a, c)) }
			}
	}
	lazy val parseCompExpr: Parser[AST] = {
			parseAddExpr ~ rep(("<="|">="|"<"|">"|"=="|"!=") ~ parseAddExpr) ^^ {
				case a~b => (a /: b) { case (a, (op~c)) => CallAST(op, Seq(a, c)) }
			}
	}
	lazy val parseExpr: Parser[AST] = parseCompExpr
	//---
	lazy val parseIfStmt: Parser[AST] = {
		"if" ~ ("(" ~> parseExpr <~ ")") ~ parseStmt ~ ("else" ~> parseStmt?) ^^ {
			case _~c~t~e => IfStmtAST(c, t, e)
		}
	}
	lazy val parseWhileStmt: Parser[AST] = {
		"while" ~ ("(" ~> parseExpr <~ ")") ~ parseStmt ^^ {
			case _~c~t => WhileStmtAST(c, t)
		}
	}
	lazy val parseBlockStmt: Parser[AST] = {
		"{" ~> rep(parseStmt) <~ "}" ^^ {
			case a => BlockStmtAST(a)
		}
	}
	lazy val parseExprStmt: Parser[AST] = parseExpr <~ ";"
	lazy val parseVarStmt: Parser[AST] = {
		parseType ~ symbol ~ "=" ~ parseExpr ~ ";" ^^ {
			case t~a~_~b~_ => VarStmtAST(t, a, b)
		}
	}
	lazy val parseStmt: Parser[AST] = {
		parseBlockStmt |
		parseIfStmt |
		parseWhileStmt |
		parseVarStmt   |
		parseExprStmt
	}
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
		val pop = args.size - func.retSize
		(("" /: args) (_ + conv(_))) + { index -= pop; func.src }
	}

	def conv(ast: AST): String = ast match {
		case NumberAST(n) => zero + convInt(n) + shift(1)
		case VarAST(s) => load(getIndex(s))
		case VarStmtAST(t, s, e) => addLocal(s); conv(e)
		case CallAST(fname, args) => convCall(fname, args)
		case BlockStmtAST(b) => b.map { conv(_) } reduce {_ + _}
		case IfStmtAST(c, t, None) =>
			conv(c) + shift(-1) + "[" + shift(1) + conv(t) + shift(-1) + "[-]]"
		case IfStmtAST(c, t, Some(e)) =>
			conv(c) + convInt(1) + shift(-1) +
			"[" + shift(2) + conv(t) + shift(-2) + "[-]" +
			">-<]" + shift(1) + "[" +
			shift(1) + conv(e) + shift(-1) + "[-]]" + shift(-1)
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

