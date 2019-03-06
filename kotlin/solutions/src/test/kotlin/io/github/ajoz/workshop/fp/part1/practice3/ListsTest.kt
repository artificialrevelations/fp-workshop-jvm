package io.github.ajoz.workshop.fp.part1.practice3

import io.github.ajoz.workshop.fp.tools.predicates.alwaysFalse
import io.github.ajoz.workshop.fp.tools.predicates.alwaysTrue
import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

import java.util.Arrays

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue

@RunWith(Enclosed::class)
class ListsTest {
    class Select {
        @Test
        fun `should return empty List for empty List`() {
            // given:
            val values = emptyList<String>()

            // when:
            val actual = Lists.select(alwaysTrue(), values)

            // then:
            assertTrue(actual.isEmpty())
        }

        @Test
        fun `should return empty List if no element satisfies predicate`() {
            // given:
            val values = listOf("JUG", "Lodz", "Workshop")

            // when:
            val actual = Lists.select(alwaysFalse(), values)

            // then:
            assertTrue(actual.isEmpty())
        }

        @Test
        fun `should return only elements that satisfy the predicate`() {
            // given:
            val values = listOf("JUG", "Lodz", "Workshop")
            val predicate: (String) -> Boolean = { it.length == 3 }
            val expected = "JUG"

            // when:
            val actual = Lists.select(predicate, values)

            // then:
            assertEquals(1, actual.size)
            assertEquals(expected, actual[0])
        }

        @Test
        fun `should return ALL elements if ALL satisfy the predicate`() {
            // given:
            val values = listOf("JUG", "Lodz", "Workshop")

            // when:
            val actual = Lists.select(alwaysTrue(), values)

            // then:
            assertEquals(values, actual)
        }
    }

    class Reject {
        @Test
        fun `should return empty List for empty List`() {
            // given:
            val values = emptyList<String>()

            // when:
            val actual = Lists.select(alwaysTrue(), values)

            // then:
            assertTrue(actual.isEmpty())
        }

        @Test
        fun `should return empty List if ALL elements satisfy the predicate`() {
            // given:
            val values = listOf("JUG", "Lodz", "Workshop")
            val predicate: (String) -> Boolean = { it.length > 2 }

            // when:
            val actual = Lists.reject(predicate, values)

            // then:
            assertTrue(actual.isEmpty())
        }

        @Test
        fun `should return ALL elements that do not satisfy the predicate`() {
            // given:
            val values = Arrays.asList("JUG", "Lodz", "Workshop")
            val predicate: (String) -> Boolean = { it.length > 3 }
            val expected = "JUG"

            // when:
            val actual = Lists.reject(predicate, values)

            // then:
            assertEquals(1, actual.size.toLong())
            assertEquals(expected, actual[0])
        }

        @Test
        fun `should return ALL elements if ALL do not satisfy the predicate`() {
            // given:
            val values = Arrays.asList("JUG", "Lodz", "Workshop")

            // when:
            val actual = Lists.reject(alwaysFalse(), values)

            // then:
            assertEquals(values, actual)
        }
    }
}
