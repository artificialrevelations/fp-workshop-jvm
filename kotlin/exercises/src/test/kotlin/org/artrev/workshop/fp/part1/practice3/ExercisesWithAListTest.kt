package org.artrev.workshop.fp.part1.practice3

import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue

@RunWith(Enclosed::class)
class ExercisesWithAListTest {
    class AllAbove42 {
        @Test
        fun `returns empty List for empty List`() {
            // given:
            val values = emptyList<Int>()

            // when:
            val actual = allAbove42(values)

            // then:
            assertTrue(actual.isEmpty())
        }

        @Test
        fun `returns empty List if ALL values are below 42`() {
            // given:
            val values = listOf(1, 2, 3)

            // when:
            val actual = allAbove42(values)

            // then:
            assertTrue(actual.isEmpty())
        }

        @Test
        fun `returns values above 42`() {
            // given:
            val values = listOf(1, 2, 3, 256)
            val expected = 256

            // when:
            val actual = allAbove42(values)

            // then:
            assertEquals(1, actual.size)
            assertEquals(expected, actual[0])
        }

        @Test
        fun `returns list with the same elements if all values are above 42`() {
            // given:
            val values = listOf(64, 128, 256, 512)

            // when:
            val actual = allAbove42(values)

            // then:
            assertEquals(values, actual)
        }
    }

    class AllBelow42 {
        @Test
        fun `returns empty List for empty List`() {
            // given:
            val values = emptyList<Int>()

            // when:
            val actual = allBelow42(values)

            // then:
            assertTrue(actual.isEmpty())
        }

        @Test
        fun `returns empty List if ALL values are above 42`() {
            // given:
            val values = listOf(64, 128, 256)

            // when:
            val actual = allBelow42(values)

            // then:
            assertTrue(actual.isEmpty())
        }

        @Test
        fun `returns values below 42`() {
            // given:
            val values = listOf(1, 64, 128, 256)
            val expected = 1

            // when:
            val actual = allBelow42(values)

            // then:
            assertEquals(1, actual.size)
            assertEquals(expected, actual[0])
        }

        @Test
        fun `returns list with the same elements if all values are below 42`() {
            // given:
            val values = listOf(1, 2, 3)

            // when:
            val actual = allBelow42(values)

            // then:
            assertEquals(values, actual)
        }
    }
}
