@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_2.solutions.exercise_3

import org.junit.Test

import java.util.Arrays

import org.junit.Assert.assertEquals

class Exercise3Test {
    @Test
    fun sum_with_foldRight_should_be_equal_to_sum_with_foldLeft() {
        // given:
        val list = Arrays.asList(1, 2, 3)

        // when:
        val sumLeft = foldLeft(list, 0) { a, b ->
            a + b
        }
        val sumRight = foldRight(list, 0) { a, b ->
            a + b
        }

        // then:
        assertEquals(sumLeft, sumRight)
    }

    @Test
    fun foldRight_structure_should_be_correct() {
        // given:
        val list = Arrays.asList(1, 2, 3)
        val expected = "(1 + (2 + (3 + 0)))"

        // when:
        val actual = foldRight(list, "0") { integer, string ->
            String.format("(%d + %s)", integer, string)
        }

        // then:
        assertEquals(expected, actual)
    }

    @Test
    fun foldRight_in_terms_of_foldLeft() {
        // given:
        val list = Arrays.asList(1, 2, 3)
        val expected = "(1 + (2 + (3 + 0)))"

        // when:
        val actual = foldRight2(list, "0") { integer, string ->
            String.format("(%d + %s)", integer, string)
        }

        // then:
        assertEquals(expected, actual)
    }
}