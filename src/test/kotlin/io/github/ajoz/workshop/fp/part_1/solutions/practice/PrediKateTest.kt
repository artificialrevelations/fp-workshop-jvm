@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.solutions.practice

import org.junit.Test

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

class PredicateTest {

    @Test
    fun trueAndFalseEqualsFalse() {
        // given:
        val true1: (String) -> Boolean = { _ -> true }
        val false1: (String) -> Boolean = { _ -> false }
        // when:
        val result = true1.and(false1)
        // then:
        assertFalse(result("testTrueAndFalse"))
    }

    @Test
    fun falseAndTrueEqualsFalse() {
        // given:
        val false1: (String) -> Boolean = { _ -> false }
        val true1: (String) -> Boolean = { _ -> true }
        // when:
        val result = false1.and(true1)
        // then:
        assertFalse(result("testFalseAndTrue"))
    }

    @Test
    fun falseAndFalseEqualsFalse() {
        // given:
        val false1: (String) -> Boolean = { _ -> false }
        val false2: (String) -> Boolean = { _ -> false }
        // when:
        val result = false1.and(false2)
        // then:
        assertFalse(result("testFalseAndFalse"))
    }

    @Test
    fun trueOrTrueEqualsTrue() {
        // given:
        val true1: (String) -> Boolean = { _ -> true }
        val true2: (String) -> Boolean = { _ -> true }
        // when:
        val result = true1.or(true2)
        // then:
        assertTrue(result("testTrueOrTrue"))
    }

    @Test
    fun trueOrFalseEqualsTrue() {
        // given:
        val true1: (String) -> Boolean = { _ -> true }
        val false1: (String) -> Boolean = { _ -> false }
        // when:
        val result = true1.or(false1)
        // then:
        assertTrue(result("testTrueOrFalse"))
    }

    @Test
    fun falseOrTrueEqualsTrue() {
        // given:
        val false1: (String) -> Boolean = { _ -> false }
        val true1: (String) -> Boolean = { _ -> true }
        // when:
        val result = false1.or(true1)
        // then:
        assertTrue(result("testFalseOrTrue"))
    }

    @Test
    fun falseOrFalseEqualsFalse() {
        // given:
        val false1: (String) -> Boolean = { _ -> false }
        val false2: (String) -> Boolean = { _ -> false }
        // when:
        val result = false1.or(false2)
        // then:
        assertFalse(result("testFalseOrFalse"))
    }
}
