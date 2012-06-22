import scala.io.Source
import scala.collection.mutable.ListBuffer

object BrainMa2 {

	case class Op {
		def merge(op: Op): Option[Op] = None
	}
	case class Nop extends Op {
		override def merge(op: Op): Option[Op] = Some(op)
	}
	case class AddOp(n: Int) extends Op {
		override def merge(op: Op): Option[Op] = op match {
			case AddOp(m) => Some(AddOp(n + m))
			case _ => None
		}
	}
	case class ShiftOp(n: Int) extends Op {
		override def merge(op: Op): Option[Op] = op match {
			case ShiftOp(m) => Some(ShiftOp(n + m))
			case _ => None
		}
	}
	case class InputOp extends Op
	case class OutputOp extends Op
	case class BlockOp(code: List[Op]) extends Op

	def addop(b: ListBuffer[Op], op: Op) =
		if(b.size > 0) b(b.size-1).merge(op) match {
			case Some(o) => b(b.size-1) = o
			case None => b += op
		} else {
			b += op
		}

	def compile(src: Iterator[Char]): List[Op] = {
		val b = new ListBuffer[Op]
		var e = true
		while(e && src.hasNext) src.next match {
			case '+' => addop(b, AddOp(1))
			case '-' => addop(b, AddOp(-1))
			case '<' => addop(b, ShiftOp(-1))
			case '>' => addop(b, ShiftOp(1))
			case '.' => addop(b, OutputOp())
			case ',' => addop(b, InputOp())
			case '[' => addop(b, BlockOp(compile(src)))
			case ']' => e = false; 
			case _   => 
		}
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
		var code = compile(src)
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

