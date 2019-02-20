@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part1.practice1

import org.junit.Assert.*
import org.junit.Test

import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

@RunWith(Enclosed::class)
class PredicateTest {
    class Part1 {
        @Test
        fun `True and False equals False`() {
            // given:
            val true1: (String) -> Boolean = { true }
            val false1: (String) -> Boolean = { false }
            // when:
            val result = true1.and(false1)
            // then:
            assertFalse(result("True and False equals False"))
        }

        @Test
        fun `False and True equals False`() {
            // given:
            val false1: (String) -> Boolean = { false }
            val true1: (String) -> Boolean = { true }
            // when:
            val result = false1.and(true1)
            // then:
            assertFalse(result("False and True equals False"))
        }

        @Test
        fun `False and False equals False`() {
            // given:
            val false1: (String) -> Boolean = { false }
            val false2: (String) -> Boolean = { false }
            // when:
            val result = false1.and(false2)
            // then:
            assertFalse(result("False and False equals False"))
        }

        @Test
        fun `True and True equals True`() {
            // given:
            val true1: (String) -> Boolean = { true }
            val true2: (String) -> Boolean = { true }
            // when:
            val result = true1.and(true2)
            // then:
            assertTrue(result("True and True equals True"))
        }
    }

    class Part2 {
        @Test
        fun `True or True equals True`() {
            // given:
            val true1: (String) -> Boolean = { true }
            val true2: (String) -> Boolean = { true }
            // when:
            val result = true1.or(true2)
            // then:
            assertTrue(result("True or True equals True"))
        }

        @Test
        fun `True or False equals True`() {
            // given:
            val true1: (String) -> Boolean = { true }
            val false1: (String) -> Boolean = { false }
            // when:
            val result = true1.or(false1)
            // then:
            assertTrue(result("True or False equals True"))
        }

        @Test
        fun `False or True equals True`() {
            // given:
            val false1: (String) -> Boolean = { false }
            val true1: (String) -> Boolean = { true }
            // when:
            val result = false1.or(true1)
            // then:
            assertTrue(result("False or True equals True"))
        }

        @Test
        fun `False or False equals False`() {
            // given:
            val false1: (String) -> Boolean = { false }
            val false2: (String) -> Boolean = { false }
            // when:
            val result = false1.or(false2)
            // then:
            assertFalse(result("False or False equals False"))
        }
    }

    class Part3 {
        @Test
        fun `not True equals False`() {
            // given:
            val aTrue: (String) -> Boolean = { true }
            // when:
            val negated = aTrue.not()
            // then:
            assertFalse(negated("not True equals False"))
        }

        @Test
        fun `not not True equals True`() {
            // given:
            val aTrue: (String) -> Boolean = { true }
            // when:
            val negated = aTrue.not().not()
            // then:
            assertTrue(negated("not not True equals True"))
        }

        @Test
        fun `not False equals True`() {
            // given:
            val aFalse: (String) -> Boolean = { false }
            // when:
            val negated = aFalse.not()
            // then:
            assertTrue(negated("not False equals True"))
        }

        @Test
        fun `not not False equals False`() {
            // given:
            val aFalse: (String) -> Boolean = { false }
            // when:
            val negated = aFalse.not().not()
            // then:
            assertFalse(negated("not not False equals False"))
        }
    }

    class Part4 {
        @Test
        fun `True xor True equals False`() {
            // given:
            val true1: (String) -> Boolean = { true }
            val true2: (String) -> Boolean = { true }
            // when:
            val result = true1.xor(true2)
            // then:
            assertFalse(result("True xor True equals False"))
        }

        @Test
        fun `True xor False equals True`() {
            // given:
            val true1: (String) -> Boolean = { true }
            val false1: (String) -> Boolean = { false }
            // when:
            val result = true1.xor(false1)
            // then:
            assertTrue(result("True xor False equals True"))
        }

        @Test
        fun `False xor True equals True`() {
            // given:
            val false1: (String) -> Boolean = { false }
            val true1: (String) -> Boolean = { true }
            // when:
            val result = false1.xor(true1)
            // then:
            assertTrue(result("False xor True equals True"))
        }

        @Test
        fun `False xor False equals False`() {
            // given:
            val false1: (String) -> Boolean = { false }
            val false2: (String) -> Boolean = { false }
            // when:
            val result = false1.xor(false2)
            // then:
            assertFalse(result("False xor False equals False"))
        }
    }
}
