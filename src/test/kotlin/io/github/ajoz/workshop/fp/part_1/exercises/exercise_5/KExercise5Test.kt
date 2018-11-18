@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.exercises.exercise_5

import org.junit.Assert
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
        Assert.assertEquals(42, tested())
    }
}