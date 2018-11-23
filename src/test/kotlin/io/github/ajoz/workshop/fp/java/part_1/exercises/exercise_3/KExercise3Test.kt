@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_3

import io.github.ajoz.workshop.fp.kotlin.part_1.exercises.exercise_3.*
import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

@RunWith(Enclosed::class)
class KExercise3Test {
    class ComposeIntFuns {
        @Test
        fun `should compose two simple functions`() {
            // given:
            val f: (Int) -> Int = { it + 1 }
            val g: (Int) -> Int = { it + 2 }

            // when:
            val h = kComposeIntFuns(f, g)

            // then:
            assertEquals(3, h(0))
            assertEquals(4, h(0))
        }

        @Test
        fun `should compose two identity functions`() {
            // given:
            val f: (Int) -> Int = { it }
            val g: (Int) -> Int = { it }

            // when:
            val h = kComposeIntFuns(f, g)

            // then:
            assertEquals(0, h(0))
            assertEquals(1, h(1))

        }

        @Test
        fun `should compose two isomorphic functions`() {
            // given:
            val f: (Int) -> Int = { it + 1 }
            val g: (Int) -> Int = { it - 1 }

            // when:
            val h = kComposeIntFuns(f, g)

            // then:
            assertEquals(0, h(0))
            assertEquals(100, h(100))
        }
    }

    class Compose {
        @Test
        fun `should compose two functions`() {
            // given:
            val f: (String) -> Int = { Integer.valueOf(it) }
            val g: (Int) -> Boolean = { it == 42 }

            // when:
            val h = kCompose(f, g)

            // then:
            assertEquals(true, h("42"))
            assertEquals(false, h("24"))
        }
    }

    class ComposeAll {
        @Test
        fun `multiple functions`() {
            // given:
            val f: (Int) -> Int = { x -> x + 1 }
            val g: (Int) -> Int = { x -> x + 2 }
            val h: (Int) -> Int = { x -> x + 3 }
            val i: (Int) -> Int = { x -> x + 4 }

            // when:
            val j = kComposeAll(f, g, h, i)

            // then:
            assertEquals(10, j(0))
            assertEquals(0, j(-10))
        }
    }

    class Function1Interface {
        @Test
        fun `should compose functions with andThen`() {
            // given:
            val f: (String) -> Int = { Integer.valueOf(it) }
            val g: (Int) -> Boolean = { it == 42 }

            // when:
            val h = f.andThen(g)

            // then:
            assertEquals(true, h("42"))
            assertEquals(false, h("24"))
        }

        @Test
        fun `should use compose`() {
            // given:
            val f: (String) -> Int = { Integer.valueOf(it) }
            val g: (Int) -> Boolean = { it == 42 }

            // when:
            val h = g.compose(f)

            // then:
            assertEquals(true, h("42"))
            assertEquals(false, h("24"))
        }
    }
}