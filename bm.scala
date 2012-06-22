import scala.io.Source
import scala.collection.mutable.ListBuffer

object BrainMa2 {

	case class Op {
		def merge(op: Op): Option[Op] = None
		def eval(sp: Int, stack: Array[Int]): Int = sp
	}
	case class Nop extends Op {
		override def merge(op: Op): Option[Op] = Some(op)
		override def eval(sp: Int, stack: Array[Int]): Int = sp
	}
	case class AddOp(n: Int) extends Op {
		override def merge(op: Op): Option[Op] = op match {
			case AddOp(m) => Some(AddOp(n + m))
			case _ => None
		}
		override def eval(sp: Int, stack: Array[Int]): Int = {
			stack(sp) += n; sp
		}
	}
	case class ShiftOp(n: Int) extends Op {
		override def merge(op: Op): Option[Op] = op match {
			case ShiftOp(m) => Some(ShiftOp(n + m))
			case _ => None
		}
		override def eval(sp: Int, stack: Array[Int]): Int = sp + n
	}
	case class InputOp extends Op {
		override def eval(sp: Int, stack: Array[Int]): Int = sp/*TODO*/
	}
	case class OutputOp extends Op {
		override def eval(sp: Int, stack: Array[Int]): Int = {
			print(stack(sp).toChar); sp
		}
	}
	case class BlockOp(code: List[Op]) extends Op {
		override def eval(sp0: Int, stack: Array[Int]): Int = {
			var sp = sp0
			while(stack(sp) != 0) code.foreach { c => sp = c.eval(sp, stack) }
			sp
		}
	}

	def compile(src: Iterator[Char]): List[Op] = {
		val b = new ListBuffer[Op]
		var e = true
		def addop(op: Op) =
			if(b.size > 0) b(b.size-1).merge(op) match {
				case Some(o) => b(b.size-1) = o
				case None => b += op
			} else {
				b += op
			}

		while(e && src.hasNext) src.next match {
			case '+' => addop(AddOp(1))
			case '-' => addop(AddOp(-1))
			case '<' => addop(ShiftOp(-1))
			case '>' => addop(ShiftOp(1))
			case '.' => addop(OutputOp())
			case ',' => addop(InputOp())
			case '[' => addop(BlockOp(compile(src)))
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

	var showCode = false

	def eval(src: Iterator[Char]) {
		var code = compile(src)
		if(showCode) {
			println("*--------------------*")
			dump(code)
			println("*--------------------*")
		}
		var sp = 0
		val stack = new Array[Int](1024)
		code foreach { c => sp = c.eval(sp, stack) }
	}

	def main(args: Array[String]) {
		var fileName: Option[String] = None
		args.foreach {
			case "-v" => println("brainma2 version 2.0"); return
			case "-i" => showCode = true
			case s    => fileName = Some(s)
		}
		fileName match {
			case Some(f) => eval(Source.fromFile(f))
			case None    => println("no input file")
		}
	} 

}

