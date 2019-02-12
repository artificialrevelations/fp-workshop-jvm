@file:Suppress("ClassName")

package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.practice_2

import io.github.ajoz.workshop.fp.java.tests.theory.Below
import io.github.ajoz.workshop.fp.kotlin.tests.theory.Above
import org.junit.experimental.theories.Theories
import org.junit.experimental.theories.Theory
import org.junit.runner.RunWith

import org.junit.Assert.assertTrue

@RunWith(Theories::class)
class Part5_6Test {
    @Theory
    fun `satisfied by larger values`(@Above(value = 42) value: Int) {
        val tested = isLargerThen(42)
        assertTrue(tested(value))
    }

    @Theory
    fun `satisfied by lower values`(@Below(value = 42) value: Int) {
        val tested = isLowerThen(42)
        assertTrue(tested(value))
    }
}
