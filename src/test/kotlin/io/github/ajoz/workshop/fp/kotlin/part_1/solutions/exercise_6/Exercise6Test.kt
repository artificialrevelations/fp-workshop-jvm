@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6

import org.junit.Assert.assertEquals
import org.junit.Test

class Exercise6Test {
    @Test
    fun `should compose function with consumer`() {
        // given:
        val consumer: (Int) -> Unit = { integer ->
            println(String.format("consumed: %d", integer))
        }

        val function: (String) -> Int = { Integer.valueOf(it) }

        // when:
        val tested = composeConsumer(function, consumer)

        // then:
        tested("42")
    }

    @Test
    fun `should compose function with supplier`() {
        // given:
        val supplier = { "42" }

        val function: (String) -> Int = { Integer.valueOf(it) }

        // when:
        val tested = composeSupplier(function, supplier)

        // then:
        assertEquals(42, tested())
    }

    @Test
    fun `should apply first argument`() {
        // given:
        val supplier = { "foo" }

        val function: (String) -> (Int) -> String = { string ->
            { integer ->
                String.format("%s : %d", string, integer)
            }
        }

        // when:
        val tested = applyCurriedFirst(function, supplier)

        // then:
        assertEquals(function("foo")(42), tested(42))
    }
}