package io.github.ajoz.workshop.fp.java.part1.practice2;

import io.github.ajoz.workshop.fp.java.tests.theory.Below;
import io.github.ajoz.workshop.fp.java.tools.Predicate;
import io.github.ajoz.workshop.fp.kotlin.tests.theory.Above;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertTrue;

@RunWith(Theories.class)
public class IntPredicatesTest {
    @Theory
    public void satisfied_by_larger_values(@Above(value = 42) final Integer value) {
        final Predicate<Integer> tested = IntPredicates.isLargerThen(42);
        assertTrue(tested.test(value));
    }

    @Theory
    public void satisfied_by_lower_values(@Below(value = 42) final Integer value) {
        final Predicate<Integer> tested = IntPredicates.isLowerThen(42);
        assertTrue(tested.test(value));
    }
}
