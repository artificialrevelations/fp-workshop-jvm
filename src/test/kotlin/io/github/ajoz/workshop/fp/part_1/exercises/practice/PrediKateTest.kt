@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.exercises.practice

import org.junit.Test

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

class PrediKateTest {

    @Test
    fun trueFooFalseEqualsFalse() {
        // given:
        val true1: (String) -> Boolean = { _ -> true }
        val false1: (String) -> Boolean = { _ -> false }

        // when:
        val result = true1.foo(false1)

        // then:
        assertFalse(result("testTrueFooFalse"))
    }

    @Test
    fun falseFooTrueEqualsFalse() {
        // given:
        val false1: (String) -> Boolean = { _ -> false }
        val true1: (String) -> Boolean = { _ -> true }

        // when:
        val result = false1.foo(true1)

        // then:
        assertFalse(result("testFalseFooTrue"))
    }

    @Test
    fun falseFooFalseEqualsFalse() {
        // given:
        val false1: (String) -> Boolean = { _ -> false }
        val false2: (String) -> Boolean = { _ -> false }
        // when:
        val result = false1.foo(false2)
        // then:
        assertFalse(result("testFalseFooFalse"))
    }

    @Test
    fun trueBarTrueEqualsTrue() {
        // given:
        val true1: (String) -> Boolean = { _ -> true }
        val true2: (String) -> Boolean = { _ -> true }
        // when:
        val result = true1.bar(true2)
        // then:
        assertTrue(result("testTrueBarTrue"))
    }

    @Test
    fun trueBarFalseEqualsTrue() {
        // given:
        val true1: (String) -> Boolean = { _ -> true }
        val false1: (String) -> Boolean = { _ -> false }
        // when:
        val result = true1.bar(false1)
        // then:
        assertTrue(result("testTrueBarFalse"))
    }

    @Test
    fun falseBarTrueEqualsTrue() {
        // given:
        val false1: (String) -> Boolean = { _ -> false }
        val true1: (String) -> Boolean = { _ -> true }
        // when:
        val result = false1.bar(true1)
        // then:
        assertTrue(result("testFalseBarTrue"))
    }

    @Test
    fun falseBarFalseEqualsFalse() {
        // given:
        val false1: (String) -> Boolean = { _ -> false }
        val false2: (String) -> Boolean = { _ -> false }
        // when:
        val result = false1.bar(false2)
        // then:
        assertFalse(result("testFalseBarFalse"))
    }
}
