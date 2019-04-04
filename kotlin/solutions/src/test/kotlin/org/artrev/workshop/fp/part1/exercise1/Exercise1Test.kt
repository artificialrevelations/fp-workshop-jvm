@file:Suppress("PackageName")

package org.artrev.workshop.fp.part1.exercise1

import org.junit.Test

import org.junit.Assert.assertEquals

class Exercise1Test {
    @Test
    fun `Test y = x + 1 function`() {
        assertEquals(42, f1(41))
    }

    @Test
    fun `Test y = (x + 2)^2 function`() {
        assertEquals(4, f2(0))
    }

    @Test
    fun `Test y = -x + 10 function`() {
        assertEquals(0, f3(10))
    }

    @Test
    fun `Test x^2 + 4x + 1 function`() {
        assertEquals(6, f4(1))
    }

    @Test
    fun `Empty String should return a 0 length`() {
        // given:
        val tested = ""

        // when:
        val result = strlen(tested)

        // then:
        assertEquals(0, result)
    }

    @Test
    fun `Single character String should return a length of 1`() {
        // given:
        val tested = "J"

        // when:
        val result = strlen(tested)

        // then:
        assertEquals(1, result)
    }

    @Test
    fun `String should return correct length`() {
        // given:
        val tested = "JUG Lodz: Check our Meetup and Facebook!!!"

        // when:
        val result = strlen(tested)

        // then:
        assertEquals(42, result)
    }
}
