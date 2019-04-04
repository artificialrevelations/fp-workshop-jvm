@file:Suppress("ClassName")

package org.artrev.workshop.fp.part1.practice2

import io.teoria.junit.numbers.Above
import io.teoria.junit.numbers.Below
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
