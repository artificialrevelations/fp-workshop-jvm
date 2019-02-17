@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_1

import io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_1.EnumBoolean.FALSE
import io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_1.EnumBoolean.TRUE
import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

import org.junit.Assert.assertEquals

@RunWith(Enclosed::class)
class EnumBooleanTest {
    class True {
        @Test
        fun `and True equals True`() {
            assertEquals(TRUE, TRUE and TRUE)
        }

        @Test
        fun `and False equals False`() {
            assertEquals(FALSE, TRUE and FALSE)
        }

        @Test
        fun `or True equals True`() {
            assertEquals(TRUE, TRUE or TRUE)
        }

        @Test
        fun `or False equals True`() {
            assertEquals(TRUE, TRUE or FALSE)
        }

        @Test
        fun `negated equals False`() {
            assertEquals(FALSE, TRUE.not())
        }
    }

    class False {
        @Test
        fun `and True equals False`() {
            assertEquals(FALSE, FALSE and TRUE)
        }

        @Test
        fun `and False equals False`() {
            assertEquals(FALSE, FALSE and FALSE)
        }

        @Test
        fun `or True equals True`() {
            assertEquals(TRUE, FALSE or TRUE)
        }

        @Test
        fun `or False equals True`() {
            assertEquals(FALSE, FALSE or FALSE)
        }

        @Test
        fun `negated equals True`() {
            assertEquals(TRUE, FALSE.not())
        }
    }
}