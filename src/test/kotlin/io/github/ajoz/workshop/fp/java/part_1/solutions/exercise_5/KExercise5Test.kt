@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_5

import io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_5.kApplyFirst
import io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_5.kComposeConsumer
import io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_5.kComposeSupplier
import org.junit.Assert.assertEquals
import org.junit.Test

class KExercise5Test {
    @Test
    fun `should compose function with consumer`() {
        // given:
        val consumer: (Int) -> Unit = { integer ->
            println(String.format("consumed: %d", integer))
        }

        val function: (String) -> Int = { Integer.valueOf(it) }

        // when:
        val tested = kComposeConsumer(function, consumer)

        // then:
        tested("42")
    }

    @Test
    fun `should compose function with supplier`() {
        // given:
        val supplier = { "42" }

        val function: (String) -> Int = { Integer.valueOf(it) }

        // when:
        val tested = kComposeSupplier(function, supplier)

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
        val tested = kApplyFirst(function, supplier)

        // then:
        assertEquals(function("foo")(42), tested(42))
    }
}