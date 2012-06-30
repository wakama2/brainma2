import scala.io.Source

object BrainMa2 {

	trait Visitor {
		def add(n: Int)
		def addTo(p: Int, k: Int)
		def addConst(p: Int, n: Int)
		def addToMul(p: Int, n: Int)
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
	case class AddToOp(n: Int, k: Int) extends Op {
		override def accept(v: Visitor) = v.addTo(n, k)
	}
	case class AddConstOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.addConst(p, n)
	}
	case class AddToMulOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.addToMul(p, n)
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
				if(n == 0) return;
				if(b.size > 0) b.last match {
					case SetOp(m)   => b.remove(b.size-1); set(n + m)
					case AddOp(m)   => b.remove(b.size-1); add(n + m)
					case ShiftOp(m) => b.remove(b.size-1); shiftAdd(m, n)
					case ShiftAddOp(m, x) => b.remove(b.size-1); shiftAdd(m, x+n)
					case AddConstOp(p, x) => b.remove(b.size-1); add(n); addConst(p, x)
					case _ => b += AddOp(n)
				} else b += AddOp(n)
			}
			def addTo(p: Int, k: Int) {
				if(b.size > 0) b.last match {
					case ShiftOp(p1) if p1+p==0 => b.remove(b.size-1); addTo(0, k+p1); shift(p1)
					case _ => b += AddToOp(p, k)
				} else b += AddToOp(p, k)
			}
			def addToMul(p: Int, n: Int) = b += AddToMulOp(p, n)
			def shift(n: Int) {
				if(n == 0) return;
				if(b.size > 0) b.last match {
					case ShiftOp(m) => b.remove(b.size-1); shift(n+m)
					case ShiftAddOp(m, x) => b.remove(b.size-1); addConst(m, x); shift(m+n)
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
			def addConst(p: Int, n: Int) {
				if(b.size > 0) b.last match {
					case ShiftOp(p1) if p1+p==0 => b.remove(b.size-1); add(n); shift(p1)
					case SetAtOp(p1, n1) if p1==p => b.remove(b.size-1); setAt(p, n+n1)
					case _ => b += AddConstOp(p, n)
				} else b += AddConstOp(p, n)
			}
			def shiftAdd(p: Int, n: Int) {
				if(b.size > 0) b.last match {
					case ShiftAddOp(m, x) => b.remove(b.size-1); addConst(m, x); shiftAdd(m+p, n)
					case _ => b += ShiftAddOp(p, n)
				} else b += ShiftAddOp(p, n)
			}
			def input  = b += InputOp()
			def output = b += OutputOp()
			def whileBlock(c: Array[Op]) {
				if(c.size >= 1) c(0) match {
					case AddOp(-1) =>
						var bb = false
						for(i <- 1 until c.size) {
							if(!c(i).isInstanceOf[AddConstOp]) bb=true
						}
						if(!bb) {
							for(i <- 1 until c.size) c(i) match {
								case AddConstOp(p, 1) => addTo(p, 0)
								case AddConstOp(p, k) => addToMul(p, k)
							}
							set(0)
							return;
						}
					case _ =>
				}
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
			def addTo(p: Int, k: Int)    = println(pref + "[%d] += [%d]".format(p, k))
			def addConst(p: Int, n: Int) = println(pref + "[%d] += %d".format(p, n))
			def addToMul(p: Int, n: Int) = println(pref + "[%d] += [0] * %d".format(p, n))
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
			def addTo(p: Int, k: Int)    = if(stack(sp+k) != 0) stack(sp + p) += stack(sp + k)
			def addConst(p: Int, n: Int) = stack(sp + p) += n
			def addToMul(p: Int, n: Int) = if(stack(sp) != 0) stack(sp + p) += stack(sp) * n
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

