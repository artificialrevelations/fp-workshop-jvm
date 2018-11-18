@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.solutions.exercise_4

import org.junit.Assert.assertEquals
import org.junit.Test

class KExercise4Test {
    @Test
    fun `should convert to Function1 with a tuple`() {
        // given:
        val add: (Int, Int) -> Int = { a, b -> a + b }
        // when:
        val tested = kConvertToFunction1WithPair(add)
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
        val tested = kConvertToFunction1WithFunction(add)
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
        val tested = kConvertToFunction2FromPair(add)
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
        val tested = convertToFunction2FromFunction(add)
        // then:
        assertEquals(add(1)(1), tested(1, 1))
        assertEquals(add(0)(0), tested(0, 0))
        assertEquals(add(-1)(1), tested(-1, 1))
    }
}
