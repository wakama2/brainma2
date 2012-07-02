import scala.io.Source

object BrainMa2 {

	trait Visitor {
		def addConst(p: Int, n: Int)        // [p] += n
		def addReg(p: Int, k: Int)          // [p] += [k]
		def addRegN(p: Int, k: Int, n: Int) // [p] += [k] * n
		def setReg(p: Int, k: Int)          // [p] = k
		def setConst(p: Int, n: Int)        // [p] = [n]
		def shift(n: Int)
		def input
		def output(n: Int)
		def outputConst(s: String)
		def whileBlock(c: Array[Op])
	}
	trait Op {
		def accept(v: Visitor): Unit
	}
	case class addRegOp(n: Int, k: Int) extends Op {
		override def accept(v: Visitor) = v.addReg(n, k)
	}
	case class AddConstOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.addConst(p, n)
	}
	case class addRegNOp(p: Int, k: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.addRegN(p, k, n)
	}
	case class ShiftOp(n: Int) extends Op {
		override def accept(v: Visitor) = v.shift(n)
	}
	case class SetConstOp(p: Int, n: Int) extends Op {
		override def accept(v: Visitor) = v.setConst(p, n)
	}
	case class SetRegOp(p: Int, k: Int) extends Op {
		override def accept(v: Visitor) = v.setReg(p, k)
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
				if(n == 0) return
				if(b.size > 0) b.last match {
					case WhileBlockOp(_) if p==0 => setConst(0, n)
					case AddConstOp(p1, n1) if p1==p => b.remove(b.size-1); addConst(p, n+n1)
					case AddConstOp(p1, n1) if p==0  => 
						b.remove(b.size-1); addConst(p, n); addConst(p1, n1)
					case ShiftOp(p1) => b.remove(b.size-1); addConst(p+p1, n); shift(p1)
					case SetConstOp(p1, n1) => b.remove(b.size-1)
						if(p1 == p) {
							setConst(p1, n+n1)
						} else {
							addConst(p, n); setConst(p1, n1)
						}
					case _ => b += AddConstOp(p, n)
				} else if(top) {
					setConst(p, n)
				} else b += AddConstOp(p, n)
			}
			def addReg(p: Int, k: Int) {
				if(top) searchConst(k, b.size-1) foreach {
					n1 => return addConst(p, n1)
				}
				if(b.size > 0) b.last match {
					case ShiftOp(p1) => b.remove(b.size-1); addReg(p+p1, k+p1); shift(p1)
					case SetConstOp(p1, 0) if p1==p => b.remove(b.size-1); setReg(p, k)
					case SetConstOp(k1, n) if k1==k => b.remove(b.size-1); addConst(p, n); setConst(k, n)
					case WhileBlockOp(_) if p==0 => setReg(0, k)
					case _ => b += addRegOp(p, k)
				} else b += addRegOp(p, k)
			}
			def addRegN(p: Int, k: Int, n: Int) {
				if(top) searchConst(k, b.size-1) foreach {
					n1 => return addConst(p, n1 * n)
				}
				if(b.size > 0) b.last match {
					case ShiftOp(p1) => b.remove(b.size-1); addRegN(p+p1, k+p1, n); shift(p1)
					case SetConstOp(p1, n1) if k==p1 => b.remove(b.size-1); addConst(p, n1 * n); setConst(p1, n1)
					case _ => b += addRegNOp(p, k, n)
				} else b += addRegNOp(p, k, n)
			}
			def shift(n: Int) {
				if(n == 0) return;
				if(b.size > 0) b.last match {
					case ShiftOp(m) => b.remove(b.size-1); shift(n+m)
					case _ => b += ShiftOp(n)
				} else b += ShiftOp(n)
			}
			def setConst(p: Int, n: Int) {
				if(b.size > 0) b.last match {
					case SetConstOp(p1, _) if p==p1 => b.remove(b.size-1); setConst(p, n)
					case ShiftOp(m) => b.remove(b.size-1); setConst(p + m, n); shift(m)
					case _ => b += SetConstOp(p, n)
				} else b += SetConstOp(p, n)
			}
			def setReg(p: Int, k: Int) {
				if(top) searchConst(k, b.size-1) foreach {
					n1 => return setConst(p, n1)
				}
				if(b.size > 0) b.last match {
					case ShiftOp(p1) => b.remove(b.size-1); setReg(p+p1, k+p1); shift(p1)
					case _ => b + SetRegOp(p, k)
				} else b += SetRegOp(p, k)
			}
			def input  = b += InputOp()
			def output(p: Int) {
				if(b.size > 0) b.last match {
					case ShiftOp(p1) => b.remove(b.size-1); output(p+p1); shift(p1)
					case SetConstOp(p1, n) => 
						b.remove(b.size-1)
						if(p1 == p) {
							outputConst(n.toChar.toString); setConst(p1, n)
						} else { 
							output(p); setConst(p1, n)
						}
					case _ => b += OutputOp(p)
				} else b += OutputOp(p)
			}
			def outputConst(s: String) {
				if(b.size > 0) b.last match {
					case SetConstOp(p1, n1) => b.remove(b.size-1); outputConst(s); setConst(p1, n1)
					case OutputConstOp(s1) => b.remove(b.size-1); outputConst(s1 + s)
					case _ => b += OutputConstOp(s)
				} else b += OutputConstOp(s)
			}
			def searchConst(p: Int, k: Int): Option[Int] = {
				for(i <- (0 to k).reverse) {
					b(i) match {
						case SetConstOp(p1, n) =>
							if(p == p1) {
								return Some(n)
							}
						case _ => return None
					}
				}
				return Some(0)
			}
			def whileBlock(c: Array[Op]) { // c.size >= 1
				if(top && b.size > 0) b.last match {
					case ShiftOp(p) => searchConst(p, b.size-2) foreach { i =>
						if(i == 0) return
						c.foreach {
							_.accept(this)
						}
						return whileBlock(c)
					}
					case _ =>
				}
				c(0) match {
					case AddConstOp(0, -1) =>
						var i = 1
						while(i < c.size && c(i).isInstanceOf[AddConstOp]) i+=1
						if(i == c.size) {
							for(j <- 1 until c.size) c(j) match {
								case AddConstOp(p, 1) => addReg(p, 0)
								case AddConstOp(p, k) => addRegN(p, 0, k)
							}
							setConst(0, 0)
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
				case SetConstOp(p, n) => b.remove(b.size-1)
				case _ => e = false
			}
		}
		b.toArray
	}

	def dump(code: Array[Op], pref: String = "") {
		new Visitor {
			def addReg(p: Int, k: Int)    = println(pref + "[%d] += [%d]".format(p, k))
			def addConst(p: Int, n: Int) = println(pref + "[%d] += %d".format(p, n))
			def addRegN(p: Int, k: Int, n: Int) = println(pref + "[%d] += [%d] * %d".format(p, k, n))
			def shift(n: Int)            = println(pref + "sp += %d".format(n))
			def setConst(p: Int, n: Int)    = println(pref + "[%d] = %d".format(p, n))
			def setReg(p: Int, k: Int)    = println(pref + "[%d] = [%d]".format(p, k))
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
			val stack = new Array[Int](10240)
			code.foreach { _.accept(this) }
			def addReg(p: Int, k: Int)    = if(stack(sp+k) != 0) stack(sp + p) += stack(sp + k)
			def addConst(p: Int, n: Int) = stack(sp + p) += n
			def addRegN(p: Int, k: Int, n: Int) = if(stack(sp+k) != 0) stack(sp + p) += stack(sp + k) * n
			def shift(n: Int) = sp += n
			def setConst(p: Int, n: Int) = stack(sp + p) = n
			def setReg(p: Int, k: Int) = stack(sp + p) = stack(sp + k)
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

