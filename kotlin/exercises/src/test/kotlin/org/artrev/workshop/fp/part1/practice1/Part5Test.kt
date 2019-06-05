package org.artrev.workshop.fp.part1.practice1

import org.artrev.teoria.numbers.ints.IntsAbove
import org.artrev.teoria.numbers.ints.IntsBelow
import org.artrev.teoria.numbers.ints.IntsBetween
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.experimental.theories.Theories
import org.junit.experimental.theories.Theory
import org.junit.experimental.theories.suppliers.TestedOn
import org.junit.runner.RunWith

@RunWith(Theories::class)
class Part5Test {
    @Theory
    fun `values between 0 and 6 are allowed`(@IntsBetween(first = 1, last = 5) value: Int) {
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
    fun `values below 0 are not allowed`(@IntsBelow(value = 0) value: Int) {
        assertFalse(isAllowed(value))
    }

    @Theory
    fun `values above 6 are not allowed`(@IntsAbove(value = 0) value: Int) {
        assertFalse(isAllowed(value))
    }
}
