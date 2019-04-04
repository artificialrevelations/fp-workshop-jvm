@file:Suppress("PackageName")

package org.artrev.workshop.fp.part1.exercise2

import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

@RunWith(Enclosed::class)
class Exercise2Test {
    class Part1 {
        @Test
        fun test_add_one() {
            // given:
            val value = 41
            val expected = 42

            // when:
            val actual = f1(value)

            // then:
            assertEquals(expected, actual)
        }

        @Test
        fun test_string_length() {
            // given:
            val value = "JUG Lodz!"
            val expected = value.length

            // when:
            val actual = f2(value)

            // then:
            assertEquals(expected, actual)
        }

        @Test
        fun test_add_foo() {
            // given:
            val value = "bar"
            val expected = "foobar"

            // when:
            val actual = f3(value)

            // then:
            assertEquals(expected, actual)
        }
    }

    class Part2 {
        @Test
        fun test_str2int() {
            // given:
            val value = "42"
            val expected = 42

            // when:
            val actual = str2int(value)

            // then:
            assertEquals(expected, actual)
        }

        @Test
        fun number_0_is_not_42() {
            // given:
            val value = 0

            // when:
            val actual = int2bool(value)

            // then:
            assertFalse(actual)
        }

        @Test
        fun number_42_is_42() {
            // given:
            val value = 42

            // when:
            val actual = int2bool(value)

            // then:
            assertTrue(actual)
        }

        @Test
        fun string_42_is_number_42_after_conversion() {
            // given:
            val value = "42"

            // when:
            val actual = str2bool(value)

            // then:
            assertTrue(actual)
        }

        @Test
        fun string_24_is_not_a_number_42_after_conversion() {
            // given:
            val value = "24"

            // when:
            val actual = str2bool(value)

            // then:
            assertFalse(actual)
        }
    }
}
