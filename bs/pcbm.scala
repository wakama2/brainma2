import util.parsing.combinator._
import util.parsing.input.CharSequenceReader
import scala.io.Source

object BrainMa2 extends Parsers {
	type Elem = Char
	val stack = new Array[Int](10000)
	var sp = 0
	
	trait Inst {
		def apply
	}
	case object NopInst extends Inst {
		def apply {}
	}
	case class AddInst(val n: Int) extends Inst {
		def apply { stack(sp) += n }
	}
	case class ShiftInst(val n: Int) extends Inst {
		def apply { sp += n }
	}
	case object OutputInst extends Inst {
		def apply { print(stack(sp).toChar) }
	}
	case object InputInst extends Inst {
		def apply { stack(sp) = Console.readChar }
	}
	case class BlockInst(val b: Seq[Inst]) extends Inst {
		def apply { while(stack(sp) != 0) b.foreach { _.apply } }
	}

	lazy val parse_plus : Parser[Inst] = elem('+') ^^^ AddInst(+1)
	lazy val parse_minus: Parser[Inst] = elem('-') ^^^ AddInst(-1)
	lazy val parse_lshift:Parser[Inst] = elem('<') ^^^ ShiftInst(-1)
	lazy val parse_rshift:Parser[Inst] = elem('>') ^^^ ShiftInst(+1)
	lazy val parse_dot  : Parser[Inst] = elem('.') ^^^ OutputInst
	lazy val parse_comma: Parser[Inst] = elem(',') ^^^ InputInst
	lazy val parse_brace: Parser[Inst] = elem('[') ~> (parse ^^ { BlockInst(_) } ) <~ elem(']')
	lazy val parse_other: Parser[Inst] = elem("", ch => ch != CharSequenceReader.EofCh && ch != ']') ^^^ NopInst
	lazy val parse: Parser[List[Inst]] = rep(parse_plus | parse_minus | parse_lshift | parse_rshift |
			parse_dot | parse_comma | parse_brace | parse_other)

	def eval(src: String) {
		parse(new CharSequenceReader(src)) match {
			case Success(a, _) => a.foreach { _.apply }
			case _ => println("error")
		}
	}

}
