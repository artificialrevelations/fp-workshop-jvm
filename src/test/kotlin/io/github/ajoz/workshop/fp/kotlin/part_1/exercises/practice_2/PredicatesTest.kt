package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.practice_2

import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

@RunWith(Enclosed::class)
class PredicatesTest {
    class Part1 {
        @Test
        fun `is not an instance of`() {
            // given:
            val tested: (String) -> Boolean =
                    instanceOf(Int::class)

            // when:
            val actual = tested("JUG Lodz!")

            // then:
            assertFalse(actual)
        }

        @Test
        fun `is an instance of`() {
            // given:
            val tested: (Int) -> Boolean =
                    instanceOf(Int::class)

            // when:
            val actual = tested(42)

            // then:
            assertTrue(actual)
        }

        @Test
        fun `is not an instance of (reified)`() {
            // given:
            val tested: (String) -> Boolean =
                    instanceOf<String, Int>()

            // when:
            val actual = tested("JUG Lodz!")

            // then:
            assertFalse(actual)
        }

        @Test
        fun `is an instance of (reified)`() {
            // given:
            val tested: (Int) -> Boolean =
                    instanceOf<Int, Int>()

            // when:
            val actual = tested(42)

            // then:
            assertTrue(actual)
        }
    }

    class Part2 {
        @Test
        fun `is always True`() {
            // given:
            val tested: (String?) -> Boolean =
                    alwaysTrue()

            // I know, I know -- single assert per test ;-)
            assertTrue(tested(null))
            assertTrue(tested(""))
            assertTrue(tested(" "))
            assertTrue(tested("    "))
            assertTrue(tested("check JUG Lodz facebook!"))
        }
    }

    class Part3 {
        @Test
        fun `is always False`() {
            // given:
            val tested: (String?) -> Boolean =
                    alwaysFalse()

            // the same as above :>
            assertFalse(tested(null))
            assertFalse(tested(""))
            assertFalse(tested(" "))
            assertFalse(tested("    "))
            assertFalse(tested("check JUG Lodz facebook!"))
        }
    }

    class Part4 {
        @Test
        fun `is equal to`() {
            // given:
            val tested = isEqualTo("foo")

            // when:
            val actual = tested("foo")

            // then:
            assertTrue(actual)
        }

        @Test
        fun `is not equal to`() {
            // given:
            val tested = isEqualTo("foo")

            // when:
            val actual = tested("bar")

            // then:
            assertFalse(actual)
        }
    }
}
