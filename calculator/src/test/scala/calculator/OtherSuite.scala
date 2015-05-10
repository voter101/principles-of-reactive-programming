
package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

@RunWith(classOf[JUnitRunner])
    class OtherSuite extends FunSuite with ShouldMatchers {

        test("should solve cyclic computations") {
            var namedExpressions: Map[String, Signal[Expr]] = Map() + ("a" -> Signal(Ref("b")))
                namedExpressions = namedExpressions + ("b" -> Signal(Ref("a")))
                // detects cyclic variables with the eval method
                assert(java.lang.Double.isNaN(Calculator.eval(Ref("a"), namedExpressions)))
                assert(java.lang.Double.isNaN(Calculator.eval(Ref("b"), namedExpressions)))

                // detects cyclic variables with the computeValues method
                var exprSignals = Calculator.computeValues(namedExpressions)
                val signalA = exprSignals.getOrElse("a", Var(0.0))
                assert(java.lang.Double.isNaN(signalA()))
                val signalB = exprSignals.getOrElse("b", Var(0.0))
                assert(java.lang.Double.isNaN(signalB()))
        }

}
