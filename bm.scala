import scala.io.Source

object BrainMa2 {

	trait Visitor {
		def add(n: Int)
		def addTo(p: Int)
		def addConst(p: Int, n: Int)
		def shift(n: Int)
		def shiftAdd(p: Int, n: Int)
		def set(n: Int)
		def setAt(p: Int, n: Int)
		def input
		def output
		def whileBlock(c: Array[Op])
	}
	trait Op {
		def accept(v: Visitor): Unit
	}
	case class AddOp(n: Int) extends Op {
		override def accept(v: Visitor) = v.add(n)
	}
	case class AddToOp(n: Int) extends Op {
		override def accept(v: Visitor) = v.addTo(n)
	}
	case class AddConstOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.addConst(p, n)
	}
	case class ShiftOp(n: Int) extends Op {
		override def accept(v: Visitor) = v.shift(n)
	}
	case class ShiftAddOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.shiftAdd(p, n)
	}
	case class SetOp(n: Int) extends Op {
		override def accept(v: Visitor) = v.set(n)
	}
	case class SetAtOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.setAt(p, n)
	}
	case class InputOp extends Op {
		override def accept(v: Visitor) = v.input
	}
	case class OutputOp extends Op {
		override def accept(v: Visitor) = v.output
	}
	case class WhileBlockOp(code: Array[Op]) extends Op {
		override def accept(v: Visitor) = v.whileBlock(code)
	}

	def compile(src: Iterator[Char]): Array[Op] = {
		val b = new scala.collection.mutable.ListBuffer[Op]
		val v = new Visitor {
			def add(n: Int) {
				if(b.size > 0) b.last match {
					case SetOp(m)   => b.remove(b.size-1); set(n + m)
					case AddOp(m)   => b.remove(b.size-1); add(n + m)
					case ShiftOp(m) => b.remove(b.size-1); shiftAdd(m, n)
					case ShiftAddOp(m, x) => b.remove(b.size-1); shiftAdd(m, x+n)
					case AddConstOp(p, x) => b.remove(b.size-1); add(n); addConst(p, x)
					case _ => b += AddOp(n)
				} else b += AddOp(n)
			}
			def addTo(p: Int) = b += AddToOp(p)
			def shift(n: Int) {
				if(b.size > 0) b.last match {
					case ShiftOp(m) => b.remove(b.size-1); shift(n+m)
					case ShiftAddOp(m, x) if m+n == 0 => b.remove(b.size-1); addConst(m, x)
					case _ => b += ShiftOp(n)
				} else b += ShiftOp(n)
			}
			def set(n: Int) {
				if(b.size > 0) b.last match {
					case ShiftOp(m) => b.remove(b.size-1); setAt(m, n); shift(m)
					case _ => b += SetOp(n)
				} else b += SetOp(n)
			}
			def setAt(p: Int, n: Int) = b += SetAtOp(p, n)
			def addConst(p: Int, n: Int) = b += AddConstOp(p, n)
			def shiftAdd(p: Int, n: Int) {
				if(b.size > 0) b.last match {
					case ShiftAddOp(m, x) => b.remove(b.size-1); addConst(m, x); shiftAdd(m+p, n)
					case _ => b += ShiftAddOp(p, n)
				} else b += ShiftAddOp(p, n)
			}
			def input  = b += InputOp()
			def output = b += OutputOp()
			def whileBlock(c: Array[Op]) {
				var f = false
				if(c.size == 2) (c(0), c(1)) match {
					case (AddOp(-1), AddConstOp(p, 1)) => addTo(p); set(0); f=true
					case (_, _) =>
				}
				if(f) return
				if(c.size == 1) c(0) match {
					case AddOp(n) => set(0)
					case _ => b += WhileBlockOp(c)
				} else b += WhileBlockOp(c)
			}
		}
		var e = true
		while(e && src.hasNext) src.next match {
			case '+' => v.add(1)
			case '-' => v.add(-1)
			case '>' => v.shift(1)
			case '<' => v.shift(-1)
			case '.' => v.output
			case ',' => v.input
			case '[' => v.whileBlock(compile(src))
			case ']' => e = false; 
			case _   => 
		}
		b.toArray
	}

	def dump(code: Array[Op], pref: String = "") {
		new Visitor {
			def add(n: Int)              = println(pref + "[0] += %d".format(n))
			def addTo(p: Int)            = println(pref + "[%d] += [0]".format(p))
			def addConst(p: Int, n: Int) = println(pref + "[%d] += %d".format(p, n))
			def shift(n: Int)            = println(pref + "sp += %d".format(n))
			def shiftAdd(p: Int, n: Int) = println(pref + "[sp += %d] += %d".format(p, n))
			def set(n: Int)              = println(pref + "[0] = %d".format(n))
			def setAt(p: Int, n: Int)    = println(pref + "[%d] = %d".format(p, n))
			def input                    = println(pref + "[0] = input")
			def output                   = println(pref + "print [0]")
			def whileBlock(c: Array[Op]) = dump(c, pref + "  ")
			code.foreach { _.accept(this) }
		}
	}

	var showCode = false

	def eval(src: Iterator[Char]) {
		val code = compile(src)
		if(showCode) {
			println("*--------------------*")
			dump(code)
			println("*--------------------*")
		}
		new Visitor {
			var sp = 0
			val stack = new Array[Int](1024)
			code.foreach { _.accept(this) }
			def add(n: Int)   = stack(sp) += n
			def addTo(p: Int) = if(stack(sp) != 0) stack(sp + p) += stack(sp)
			def addConst(p: Int, n: Int) = stack(sp + p) += n
			def shift(n: Int) = sp += n
			def shiftAdd(p: Int, n: Int) = { sp += p; stack(sp) += n }
			def set(n: Int)   = stack(sp) = n
			def setAt(p: Int, n: Int) = stack(sp + p) = n
			def input         = { /* TODO */ }
			def output        = print(stack(sp).toChar)
			def whileBlock(c: Array[Op]) {
				while(stack(sp) != 0) c.foreach { _.accept(this) }
			}
		}
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

