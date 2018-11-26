@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_2.solutions.exercise_5

import org.junit.Assert.assertEquals
import org.junit.Test
import java.util.*

class Exercise5Test {
    @Test
    fun should_map_ints() {
        // given:
        val ints = Arrays.asList(1, 2, 3)
        val expected = Arrays.asList(2, 3, 4)
        val addOne: (Int) -> Int = { x -> x + 1 }

        // when:
        val actual = mapInts(ints, addOne)

        // then:
        assertEquals(expected, actual)
    }

    @Test
    fun should_map_a_list() {
        // given:
        val ints = Arrays.asList(1, 2, 3)
        val expected = Arrays.asList("1", "2", "3")
        val toString: (Int) -> String = { x -> "$x" }

        // when:
        val actual = map(ints, toString)

        // then:
        assertEquals(expected, actual)
    }

    @Test
    fun test_addOne() {
        // given:
        val ints = Arrays.asList(1, 2, 3)
        val expected = Arrays.asList(2, 3, 4)

        // when:
        val actual = addOne(ints)

        // then:
        assertEquals(expected, actual)
    }

    @Test
    fun test_lengths() {
        // given:
        val strings = Arrays.asList("JUG", "Lodz")
        val expected = Arrays.asList(3, 4)

        // when:
        val actual = lengths(strings)

        // then:
        assertEquals(expected, actual)
    }
}
