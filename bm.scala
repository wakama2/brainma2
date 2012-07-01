import scala.io.Source

object BrainMa2 {

	trait Visitor {
		def addTo(p: Int, k: Int)
		def addConst(p: Int, n: Int)
		def addToMul(p: Int, k: Int, n: Int) // [p] += [k] * n
		def shift(n: Int)
		def setAt(p: Int, n: Int)
		def setTo(p: Int, k: Int)
		def input
		def output(n: Int)
		def outputConst(s: String)
		def whileBlock(c: Array[Op])
	}
	trait Op {
		def accept(v: Visitor): Unit
	}
	case class AddToOp(n: Int, k: Int) extends Op {
		override def accept(v: Visitor) = v.addTo(n, k)
	}
	case class AddConstOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.addConst(p, n)
	}
	case class AddToMulOp(p: Int, k: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.addToMul(p, k, n)
	}
	case class ShiftOp(n: Int) extends Op {
		override def accept(v: Visitor) = v.shift(n)
	}
	case class SetAtOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.setAt(p, n)
	}
	case class SetToOp(p: Int, k: Int) extends Op {
		override def accept(v: Visitor) = v.setTo(p, k)
	}
	case class InputOp extends Op {
		override def accept(v: Visitor) = v.input
	}
	case class OutputOp(p: Int) extends Op {
		override def accept(v: Visitor) = v.output(p)
	}
	case class OutputConstOp(p: String) extends Op {
		override def accept(v: Visitor) = v.outputConst(p)
	}
	case class WhileBlockOp(code: Array[Op]) extends Op {
		override def accept(v: Visitor) = v.whileBlock(code)
	}

	def compile(src: Iterator[Char], top: Boolean = true): Array[Op] = {
		val b = new scala.collection.mutable.ListBuffer[Op]
		val v = new Visitor {
			def addConst(p: Int, n: Int) {
				if(b.size > 0) b.last match {
					case WhileBlockOp(_) if p==0 => setAt(0, n)
					case AddConstOp(p1, n1) if p1==p => b.remove(b.size-1); addConst(p, n+n1)
					case AddConstOp(p1, n1) if p==0  => 
						b.remove(b.size-1); addConst(p, n); addConst(p1, n1)
					case ShiftOp(p1) => b.remove(b.size-1); addConst(p+p1, n); shift(p1)
					case SetAtOp(p1, n1) => b.remove(b.size-1)
						if(p1 == p) {
							setAt(p1, n+n1)
						} else {
							addConst(p, n); setAt(p1, n1)
						}
					case _ => b += AddConstOp(p, n)
				} else if(top) {
					setAt(p, n)
				} else b += AddConstOp(p, n)
			}
			def addTo(p: Int, k: Int) {
				if(b.size > 0) b.last match {
					case ShiftOp(p1) => b.remove(b.size-1); addTo(p+p1, k+p1); shift(p1)
					case SetAtOp(p1, 0) if p1==p => b.remove(b.size-1); setTo(p, k)
					case SetAtOp(k1, n) if k1==k => b.remove(b.size-1); addConst(p, n); setAt(k, n)
					case WhileBlockOp(_) if p==0 => setTo(0, k)
					case _ => b += AddToOp(p, k)
				} else b += AddToOp(p, k)
			}
			def addToMul(p: Int, k: Int, n: Int) {
				if(b.size > 0) b.last match {
					case ShiftOp(p1) => b.remove(b.size-1); addToMul(p+p1, k+p1, n); shift(p1)
					case SetAtOp(p1, n1) if k==p1 => b.remove(b.size-1); addConst(p, n1 * n); setAt(p1, n1)
					case _ => b += AddToMulOp(p, k, n)
				} else b += AddToMulOp(p, k, n)
			}
			def shift(n: Int) {
				if(n == 0) return;
				if(b.size > 0) b.last match {
					case ShiftOp(m) => b.remove(b.size-1); shift(n+m)
					case _ => b += ShiftOp(n)
				} else b += ShiftOp(n)
			}
			def setAt(p: Int, n: Int) {
				if(b.size > 0) b.last match {
					case SetAtOp(p1, _) if p==p1 => b.remove(b.size-1); setAt(p, n)
					case ShiftOp(m) => b.remove(b.size-1); setAt(p + m, n); shift(m)
					case _ => b += SetAtOp(p, n)
				} else b += SetAtOp(p, n)
			}
			def setTo(p: Int, k: Int) = b += SetToOp(p, k)
			def input  = b += InputOp()
			def output(p: Int) {
				if(b.size > 0) b.last match {
					case ShiftOp(p1) => b.remove(b.size-1); output(p+p1); shift(p1)
					case SetAtOp(p1, n) => 
						b.remove(b.size-1)
						if(p1 == p) {
							outputConst(n.toChar.toString); setAt(p1, n)
						} else { 
							output(p); setAt(p1, n)
						}
					case _ => b += OutputOp(p)
				} else b += OutputOp(p)
			}
			def outputConst(s: String) {
				if(b.size > 0) b.last match {
					case SetAtOp(p1, n1) => b.remove(b.size-1); outputConst(s); setAt(p1, n1)
					case OutputConstOp(s1) => b.remove(b.size-1); outputConst(s1 + s)
					case _ => b += OutputConstOp(s)
				} else b += OutputConstOp(s)
			}
			def whileBlock(c: Array[Op]) { // c.size >= 1
				c(0) match {
					case AddConstOp(0, -1) =>
						var i = 1
						while(i < c.size && c(i).isInstanceOf[AddConstOp]) i+=1
						if(i == c.size) {
							for(j <- 1 until c.size) c(j) match {
								case AddConstOp(p, 1) => addTo(p, 0)
								case AddConstOp(p, k) => addToMul(p, 0, k)
							}
							setAt(0, 0)
							return;
						}
					case _ =>
				}
				b += WhileBlockOp(c)
			}
		}
		var e = true
		while(e && src.hasNext) src.next match {
			case '+' => v.addConst(0, 1)
			case '-' => v.addConst(0, -1)
			case '>' => v.shift(1)
			case '<' => v.shift(-1)
			case '.' => v.output(0)
			case ',' => v.input
			case '[' => v.whileBlock(compile(src, false))
			case ']' => e = false; 
			case _   => 
		}
		if(top) {
			e = true
			while(e && b.size > 0) b(b.size-1) match {
				case AddConstOp(p, n) => b.remove(b.size-1)
				case ShiftOp(n) => b.remove(b.size-1)
				case SetAtOp(p, n) => b.remove(b.size-1)
				case _ => e = false
			}
		}
		b.toArray
	}

	def dump(code: Array[Op], pref: String = "") {
		new Visitor {
			def addTo(p: Int, k: Int)    = println(pref + "[%d] += [%d]".format(p, k))
			def addConst(p: Int, n: Int) = println(pref + "[%d] += %d".format(p, n))
			def addToMul(p: Int, k: Int, n: Int) = println(pref + "[%d] += [%d] * %d".format(p, k, n))
			def shift(n: Int)            = println(pref + "sp += %d".format(n))
			def setAt(p: Int, n: Int)    = println(pref + "[%d] = %d".format(p, n))
			def setTo(p: Int, k: Int)    = println(pref + "[%d] = [%d]".format(p, k))
			def input                    = println(pref + "[0] = input")
			def output(p: Int)           = println(pref + "print [%d]".format(p))
			def outputConst(n: String)   = println(pref + "print %s".format(n))
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
			def addTo(p: Int, k: Int)    = if(stack(sp+k) != 0) stack(sp + p) += stack(sp + k)
			def addConst(p: Int, n: Int) = stack(sp + p) += n
			def addToMul(p: Int, k: Int, n: Int) = if(stack(sp+k) != 0) stack(sp + p) += stack(sp + k) * n
			def shift(n: Int) = sp += n
			def setAt(p: Int, n: Int) = stack(sp + p) = n
			def setTo(p: Int, k: Int) = stack(sp + p) = stack(sp + k)
			def input          = { /* TODO */ }
			def output(p: Int) = print(stack(sp + p).toChar)
			def outputConst(n: String) = print(n)
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

