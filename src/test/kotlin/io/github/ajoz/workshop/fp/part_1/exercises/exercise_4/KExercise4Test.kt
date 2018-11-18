@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.exercises.exercise_4

import org.junit.Assert.assertEquals
import org.junit.Test

class KExercise4Test {
    @Test
    fun `should convert to Function1 with a tuple`() {
        // given:
        val add: (Int, Int) -> Int = { a, b -> a + b }
        // when:
        val tested = kTuple(add)
        // then:
        assertEquals(add(1, 1), tested(Pair(1, 1)))
        assertEquals(add(0, 0), tested(Pair(0, 0)))
        assertEquals(add(-1, 1), tested(Pair(-1, 1)))
    }

    @Test
    fun `should convert to Function1 with a Function1 return type`() {
        // given:
        val add: (Int, Int) -> Int = { a, b -> a + b }
        // when:
        val tested = kCurry(add)
        // then:
        assertEquals(add(1, 1), tested(1)(1))
        assertEquals(add(0, 0), tested(0)(0))
        assertEquals(add(-1, 1), tested(-1)(1))
    }

    @Test
    fun `should convert to Function2 from a Function1 with a tuple`() {
        // given:
        val add: (Pair<Int, Int>) -> Int = { ab_pair -> ab_pair.first + ab_pair.second }
        // when:
        val tested = kUnTuple(add)
        // then:
        assertEquals(add(Pair(1, 1)), tested(1, 1))
        assertEquals(add(Pair(0, 0)), tested(0, 0))
        assertEquals(add(Pair(-1, 1)), tested(-1, 1))
    }

    @Test
    fun `should convert to Function2 from a Function1 with a Function1 as return type`() {
        // given:
        val add: (Int) -> (Int) -> Int = { a -> { b -> a + b } }
        // when:
        val tested = kUnCurry(add)
        // then:
        assertEquals(add(1)(1), tested(1, 1))
        assertEquals(add(0)(0), tested(0, 0))
        assertEquals(add(-1)(1), tested(-1, 1))
    }

    @Test
    fun `should flip Function2`() {
        // given:
        val function: (String, Int) -> String = { string, integer ->
            String.format("%s : %d", string, integer)
        }

        // when:
        val tested = kFlip(function)

        // then:
        assertEquals(function("foo", 42), tested(42, "foo"))
    }

    @Test
    fun `should flip Function1 with a Tuple argument`() {
        // given:
        val function: (Pair<String, Int>) -> String = { pair ->
            String.format("%s : %d", pair.first, pair.second)
        }

        // when:
        val tested = kFlipTupled(function)

        // then:
        assertEquals(function(Pair("foo", 42)), tested(Pair(42, "foo")))
    }

    @Test
    fun `should flip curried function`() {
        // given:
        val function: (String) -> (Int) -> String = { string ->
            { integer ->
                String.format("%s : %d", string, integer)
            }
        }

        // when:
        val tested = kFlipCurried(function)

        // then:
        assertEquals(function("foo")(42), tested(42)("foo"))
    }
}
