package io.github.ajoz.workshop.fp.part1.practice1

import io.github.ajoz.workshop.fp.tests.theory.Above
import io.github.ajoz.workshop.fp.tests.theory.Below
import io.github.ajoz.workshop.fp.tests.theory.Between
import org.junit.experimental.theories.Theories
import org.junit.experimental.theories.Theory
import org.junit.experimental.theories.suppliers.TestedOn
import org.junit.runner.RunWith

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue

@RunWith(Theories::class)
class Part5Test {
    @Theory
    fun `values between 0 and 6 are allowed`(@Between(first = 1, last = 5) value: Int) {
        assertTrue(isAllowed(value))
    }

    @Theory
    fun `value 42 is allowed`(@TestedOn(ints = [42]) value: Int) {
        assertTrue(isAllowed(value))
    }

    @Theory
    fun `values 0 and 6 are not allowed`(@TestedOn(ints = [0, 6]) value: Int) {
        assertFalse(isAllowed(value))
    }

    @Theory
    fun `values below 0 are not allowed`(@Below(value = 0) value: Int) {
        assertFalse(isAllowed(value))
    }

    @Theory
    fun `values above 6 are not allowed`(@Above(value = 0) value: Int) {
        assertFalse(isAllowed(value))
    }
}
