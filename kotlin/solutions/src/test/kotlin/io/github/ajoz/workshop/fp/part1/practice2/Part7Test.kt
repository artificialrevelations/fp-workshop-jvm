package io.github.ajoz.workshop.fp.part1.practice2

import org.junit.Test
import org.junit.experimental.runners.Enclosed
import org.junit.runner.RunWith

import org.junit.Assert.assertTrue

@RunWith(Enclosed::class)
class Part7Test {
    class Part7 {
        // we have property based testing for the methods isLowerThen
        // and isLarger then used by the isAllowed implementation
        // we can just do a simple check here
        @Test
        fun `value 3 is allowed`() {
            assertTrue(isAllowed(3))
        }

        @Test
        fun `value 42 is allowed`() {
            assertTrue(isAllowed(42))
        }
    }
}
