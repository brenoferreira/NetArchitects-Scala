import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import lists._

class ListTests extends FunSpec with ShouldMatchers {

  describe("An empty list") {

    it("should be empty") {
    	val list = Empty
    	list.isEmpty should be (true)
    }

    it("head should throw exception") {
    	val list = Empty
    	evaluating { list.head } should produce [Exception]
    }

    it("tail should throw exception") {
    	val list = Empty
    	evaluating { list.tail } should produce [Exception]
    }

    it("length should be 0") {
    	val list = Empty
    	list.length should be (0)
    }

    it("last should throw exception") {
    	val list = Empty
    	evaluating { list.last } should produce [Exception]
    }

    it("drop should throw exception") {
    	val list = Empty
    	evaluating { list.drop(1) } should produce [Exception]
    }
  }

  describe("Cons list") {
  	it("should not be empty") {
  		val list = new Cons(1, Empty)

  		list.isEmpty should be (false)
  	}

  	it("head should equal param") {
  		val list = new Cons(1, Empty)

  		list.head should be (1)
  	}

  	it("tail should equal param") {
  		val empty = Empty
  		val list = new Cons(1, empty)

  		list.tail should be (empty)
  	}

  	it("head of tail should equal param") {
  		val empty = Empty
  		val list = new Cons(2, new Cons(1, empty))

  		list.tail.head should be (1)
  	}

  	it("length should be one for list with one element") {
  		val empty = Empty
  		val list = new Cons(1, empty)

  		list.length should be (1)
  	}

  	it("length should be two for list with two element") {
  		val empty = Empty
  		val list = new Cons(2, new Cons(1, empty))

  		list.length should be (2)
  	}

  	it("last of list(2, 1) should return 1") {
  		val empty = Empty
  		val list = new Cons(2, new Cons(1, empty))

  		list.last should be (1)
  	}

  	it("last of list(1, 2) should return 2") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(2, empty))

  		list.last should be (2)
  	}

  	it("last of list(1, 3) should return 3") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(3, empty))

  		list.last should be (3)
  	}

  	it("drop 1 of list(1, 2) returns new list of length 1") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(2, empty))

  		val newList = list drop 1

  		newList.length should be (1)
  	}

  	it("drop 2 of list(1, 2, 3) returns new list of length 1") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(2, new Cons(3, empty)))

  		val newList = list drop 2

  		newList.length should be (1)
  	}

  	it("drop 2 of list(1, 2, 3) returns new list(3)") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(2, new Cons(3, empty)))

  		val newList = list drop 2

  		newList.head should be (3)
  	}

  	it("drop 2 of list(1, 2, 3) returns new list with empty tail") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(2, new Cons(3, empty)))

  		val newList = list drop 2

  		newList.tail should be (empty)
  	}

  	it("take 2 of list(1, 2, 3) returns new list of length 2") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(2, new Cons(3, empty)))

  		val newList = list take 2

  		newList.length should be (2)
  	}

  	it("take 2 of list(1, 2, 3) returns new list with head 1") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(2, new Cons(3, empty)))

  		val newList = list take 2

  		newList.head should be (1)
  	}

  	it("take 2 of list(1, 2, 3) returns new list with tail - head 2") {
  		val empty = Empty
  		val list = new Cons(1, new Cons(2, new Cons(3, empty)))

  		val newList = list take 2

  		newList.tail.head should be (2)
  	}

    it("map to square should return list of squared numbers") {
      val empty = Empty
      val list = new Cons(1, new Cons(2, new Cons(3, empty))) with FMap[Int, Int]

      val squared = list.map(list)((x) => x * x)

      squared.head should be (1)
      squared.tail.head should be (4)
      squared.tail.tail.head should be (9)
    }
  }
}
