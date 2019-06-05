@file:Suppress("ClassName")

package org.artrev.workshop.fp.part1.practice2

import org.artrev.teoria.numbers.ints.IntsAbove
import org.artrev.teoria.numbers.ints.IntsBelow
import org.junit.experimental.theories.Theories
import org.junit.experimental.theories.Theory
import org.junit.runner.RunWith

import org.junit.Assert.assertTrue

@RunWith(Theories::class)
class Part5_6Test {
    @Theory
    fun `satisfied by larger values`(@IntsAbove(value = 42) value: Int) {
        val tested = isLargerThen(42)
        assertTrue(tested(value))
    }

    @Theory
    fun `satisfied by lower values`(@IntsBelow(value = 42) value: Int) {
        val tested = isLowerThen(42)
        assertTrue(tested(value))
    }
}
