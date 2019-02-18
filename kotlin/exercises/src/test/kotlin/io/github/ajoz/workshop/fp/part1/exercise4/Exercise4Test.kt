package io.github.ajoz.workshop.fp.part1.exercise4

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

@RunWith(Enclosed::class)
class Exercise4Test {
    class Tuple {
        @Test
        fun `should convert to a one argument function`() {
            // given:
            val add: (Int, Int) -> Int = { a, b -> a + b }
            // when:
            val tested = tuple(add)
            // then:
            assertEquals(add(1, 1), tested(Pair(1, 1)))
            assertEquals(add(0, 0), tested(Pair(0, 0)))
            assertEquals(add(-1, 1), tested(Pair(-1, 1)))
        }

        @Test
        fun `should convert to a two argument function`() {
            // given:
            val add: (Pair<Int, Int>) -> Int = { ab_pair -> ab_pair.first + ab_pair.second }
            // when:
            val tested = unTuple(add)
            // then:
            assertEquals(add(Pair(1, 1)), tested(1, 1))
            assertEquals(add(Pair(0, 0)), tested(0, 0))
            assertEquals(add(Pair(-1, 1)), tested(-1, 1))
        }
    }

    class Curry {
        @Test
        fun `should convert a two argument function`() {
            // given:
            val add: (Int, Int) -> Int = { a, b -> a + b }
            // when:
            val tested = curry(add)
            // then:
            assertEquals(add(1, 1), tested(1)(1))
            assertEquals(add(0, 0), tested(0)(0))
            assertEquals(add(-1, 1), tested(-1)(1))
        }

        @Test
        fun `should convert back to a two argument function`() {
            // given:
            val add: (Int) -> (Int) -> Int = { a -> { b -> a + b } }
            // when:
            val tested = unCurry(add)
            // then:
            assertEquals(add(1)(1), tested(1, 1))
            assertEquals(add(0)(0), tested(0, 0))
            assertEquals(add(-1)(1), tested(-1, 1))
        }
    }

    class Flip {
        @Test
        fun `should flip a two argument function`() {
            // given:
            val function: (String, Int) -> String = { string, integer ->
                String.format("%s : %d", string, integer)
            }

            // when:
            val tested = flip(function)

            // then:
            assertEquals(function("foo", 42), tested(42, "foo"))
        }

        @Test
        fun `should flip a one argument function with a Tuple argument`() {
            // given:
            val function: (Pair<String, Int>) -> String = { pair ->
                String.format("%s : %d", pair.first, pair.second)
            }

            // when:
            val tested = flipTupled(function)

            // then:
            assertEquals(function(Pair("foo", 42)), tested(Pair(42, "foo")))
        }

        @Test
        fun `should flip a curried function`() {
            // given:
            val function: (String) -> (Int) -> String = { string ->
                { integer ->
                    String.format("%s : %d", string, integer)
                }
            }

            // when:
            val tested = flipCurried(function)

            // then:
            assertEquals(function("foo")(42), tested(42)("foo"))
        }
    }
}
