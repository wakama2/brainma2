import scala.io.Source
import scala.collection.mutable.ListBuffer

object BrainMa2 {

	case class Op
	case class Nop extends Op
	case class AddOp(n: Int) extends Op
	case class ShiftOp(n: Int) extends Op
	case class InputOp extends Op
	case class OutputOp extends Op
	case class BlockOp(code: List[Op]) extends Op

	def compile(src: Iterator[Char]): List[Op] = {
		val b = new ListBuffer[Op]
		var e = true
		while(e && src.hasNext) src.next match {
			case '+' => b += AddOp(1)
			case '-' => b += AddOp(-1)
			case '<' => b += ShiftOp(-1)
			case '>' => b += ShiftOp(1)
			case '.' => b += OutputOp()
			case ',' => b += InputOp()
			case '[' => b += BlockOp(compile(src))
			case ']' => e = false; 
			case _   => 
		}
		b.toList
	}

	def opt(code: List[Op]): List[Op] = {
		val b = new ListBuffer[Op]
		var prev: Op = null
		code.foreach { op =>
			(prev, op) match {
				case (AddOp(x), AddOp(y)) => prev = AddOp(x + y)
				case (ShiftOp(x), ShiftOp(y)) => prev = ShiftOp(x + y)
				case (Op(), bop: BlockOp) => b += prev; prev = BlockOp(opt(bop.code))
				case (Op(), Op()) => b += prev; prev = op
				case (null, _) => prev = op
			}
		}
		if(prev != null) b += prev
		b.toList
	}

	def dump(code: List[Op], indent: Int = 0) {
		code.foreach { op =>
			op match {
				case op: BlockOp => dump(op.code, indent + 1)
				case op: Op => println("  " * indent + op)
			}
		}
	}

	def run(code: List[Op]) {
		val stack = new Array[Int](1024)
		var sp = 0
		def runBlock(code: List[Op]) {
			code.foreach {
				case Nop()      =>
				case AddOp(n)   => stack(sp) += n
				case ShiftOp(n) => sp += n
				case InputOp()  => /* TODO */
				case OutputOp() => print(stack(sp).toChar)
				case BlockOp(c) => while(stack(sp) != 0) runBlock(c)
			}
		}
		runBlock(code)
	}

	class Context {
		var showCode = false
	}

	def eval(src: Iterator[Char], ctx: Option[Context] = None) {
		var code = opt(compile(src))
		ctx.foreach { c =>
			if(c.showCode) {
				println("*--------------------*")
				dump(code)
				println("*--------------------*")
			}
		}
		run(code)
	}

	def main(args: Array[String]) {
		var fileName: Option[String] = None
		val ctx = new Context
		args.foreach {
			case "-v" => println("brainma2 version 2.0"); return
			case "-i" => ctx.showCode = true
			case s    => fileName = Some(s)
		}
		fileName match {
			case Some(f) => eval(Source.fromFile(f), Some(ctx))
			case None    => println("no input file")
		}
	} 

}

