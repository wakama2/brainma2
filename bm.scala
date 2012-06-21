import scala.io.Source

object BrainMa2 {

	case class Op
	case class Nop extends Op
	case class AddOp(n: Int) extends Op
	case class ShiftOp(n: Int) extends Op
	case class InputOp extends Op
	case class OutputOp extends Op
	case class BlockOp(code: List[Op]) extends Op

	def compile(src: Iterator[Char]): List[Op] = {
		val b = new scala.collection.mutable.ListBuffer[Op]
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

	def dump(code: List[Op], indent: Int = 0) {
		code.foreach { o =>
			(0 until indent) foreach { i => print("\t") }
			o match {
				case op: BlockOp => dump(op.code, indent + 1)
				case op: Op => println(op)
			}
		}
	}

	def run(code: List[Op]) {
		val stack = new Array[Int](1024)
		var sp = 0
		def runBlock(code: List[Op]) {
			code.foreach {
				case AddOp(n) => stack(sp) += n
				case ShiftOp(n) => sp += n
				case OutputOp() => print(stack(sp).toChar)
				case BlockOp(c) => while(stack(sp) != 0) runBlock(c)
			}
		}
		runBlock(code)
	}

	def eval(src: Iterator[Char]) {
		val code = compile(src)
		println("*--------------------*")
		dump(code)
		println("*--------------------*")
		run(code)
	}

	def main(args: Array[String]) {
		if(args.length >= 1) {
			eval(Source.fromFile(args(0)))
		} else {
			println("no input file")
		}
	} 

}

