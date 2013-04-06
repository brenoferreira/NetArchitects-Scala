package lists

trait FMap[A, B] {
	def map[A, B](list: List[A])(f: (A) => B):List[B] = {
		if(list.isEmpty) Empty
		else new Cons(f(list.head), map(list.tail)(f))
	}
}

abstract class List[+A] {
	def isEmpty:Boolean
	def head:A
	def tail: List[A]
	def length:Int
	def last:A
	def drop(n:Int): List[A]
	def take(n:Int): List[A]
}

object Empty extends List {
	def isEmpty = true
	def head = throw new Exception()
	def tail = throw new Exception()
	def length = 0
	def last = throw new Exception()
	def drop(n:Int) = throw new Exception()
	def take(n:Int) = this
}

case class Cons[A](head: A, tail: List[A]) extends List[A] {
	def isEmpty = false

	def length =
		isEmpty match {
			case true => 0
			case false => 1 + tail.length
		}

	def last: A =
		tail.isEmpty match {
			case true => head
			case false => tail.last
		}

	def drop(n:Int): List[A] = {
		if(isEmpty) Empty
		else if(n <= 0) this
		else tail.drop(n - 1)
	}

	def take(n:Int): List[A] = {
		if(n <= 0) Empty
		else new Cons(head, tail.take(n-1))
	}
}