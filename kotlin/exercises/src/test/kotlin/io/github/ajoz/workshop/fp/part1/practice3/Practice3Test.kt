package io.github.ajoz.workshop.fp.part1.practice3

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class Practice3Test {
    @Test
    fun `returns empty List for an empty List`() {
        // given:
        val values = emptyList<String>()

        // when:
        val actual = getStrings(values)

        // then:
        assertTrue(actual.isEmpty())
    }

    @Test
    fun `returns empty List if no strings are correct`() {
        // given:
        val values = listOf(
                "", // one that is empty
                "aJUGa", //one that has JUG but no capital first or last
                "C++++", //one that has a capital first but no JUG
                "c++++J", //one that has a capital last but no JUG
                "JUG", //one that has JUG and first or last capital but is not larger then 3
                "C12345JUG6789D" // one that has JUG, first or last capital but is not smaller then 10
        )

        // when:
        val actual = getStrings(values)

        // then:
        assertTrue(actual.isEmpty())
    }

    @Test
    fun `returns List with Strings that satisfy the predicate`() {
        // given:
        val values = listOf(
                "",
                "JUG is NOT cool!!!!",
                "JUG=cool",
                "This workshop sucks",
                "JUG Lodz",
                "C12345JUG6789D",
                "JoinJUG"
        )

        val expected = listOf(
                "JUG=cool",
                "JUG Lodz",
                "JoinJUG"
        )

        // when:
        val actual = getStrings(values)

        // then:
        assertEquals(expected, actual)
    }
}
