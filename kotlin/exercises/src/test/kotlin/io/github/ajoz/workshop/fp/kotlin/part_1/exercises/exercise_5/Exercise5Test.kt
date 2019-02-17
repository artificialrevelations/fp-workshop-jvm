package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.exercise_5

import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

import org.junit.Assert.assertEquals

@RunWith(Enclosed::class)
class Exercise5Test {
    class Part1 {
        @Test
        fun `should apply only the first argument`() {
            // given:
            val first = 1
            val second = 1
            val expected = 2

            val curried: (Int) -> (Int) -> Int =
                    { a -> { b -> a + b } }

            val tested = curried.applyFirst(first)

            // when:
            val actual = tested(second)

            // then:
            assertEquals(expected, actual)
        }
    }

    class Part2 {
        @Test
        fun `should apply only the second argument`() {
            // given:
            val first = "foobar"
            val second = 42
            val expected = "foobar42"

            val curried: (String) -> (Int) -> String =
                    { string -> { integer -> string + integer } }

            val tested = curried.applySecond(second)

            // when:
            val actual = tested(first)

            // then:
            assertEquals(expected, actual)
        }
    }

    class Part4 {
        @Test
        fun `should apply only the first argument`() {
            // given:
            val first = 1
            val second = 1
            val expected = 2

            val add: (Int, Int) -> Int =
                    { a, b -> a + b }

            val tested = add.applyFirst(first)

            // when:
            val actual = tested(second)

            // then:
            assertEquals(expected, actual)
        }
    }

    class Part5 {
        @Test
        fun `should apply only the second argument`() {
            // given:
            val first = "foobar"
            val second = 42
            val expected = "foobar42"

            val concat: (String, Int) -> String =
                    { string, integer -> string + integer }

            val tested = concat.applySecond(second)

            // when:
            val actual = tested(first)

            // then:
            assertEquals(expected, actual)
        }
    }
}
