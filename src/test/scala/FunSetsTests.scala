import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import funsets._

class FunSetsTests extends FunSpec with ShouldMatchers {

	describe("FunSets") {

		it("singleton set should return true") {
			var set = FunSets.singletonSet(2);

			set(2) should be (true)
		}

	}

}