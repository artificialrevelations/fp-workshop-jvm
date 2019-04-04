@file:Suppress("PackageName")

package org.artrev.workshop.fp.part1.exercise6

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
        val tested = consumer.compose(function)

        // then:
        tested("42")
    }

    @Test
    fun `should compose function with the supplier`() {
        // given:
        val supplier = { "42" }

        val function: (String) -> Int = { Integer.valueOf(it) }

        // when:
        val tested = supplier.andThen(function)

        // then:
        assertEquals(42, tested())
    }

    @Test
    fun `should apply the first argument`() {
        // given:
        val supplier = { "foo" }
        val function: (String) -> (Int) -> String = { string ->
            { integer ->
                String.format("%s : %d", string, integer)
            }
        }

        // when:
        val tested = function.applyFirst(supplier)

        // then:
        assertEquals(function("foo")(42), tested(42))
    }

    @Test
    fun `should apply the second argument`() {
        val supplier = { 42 }
        val function: (String) -> (Int) -> String = { string ->
            { integer ->
                String.format("%s : %d", string, integer)
            }
        }

        // when:
        val tested = function.applySecond(supplier)

        // then:
        assertEquals(function("foo")(42), tested("foo"))
    }
}