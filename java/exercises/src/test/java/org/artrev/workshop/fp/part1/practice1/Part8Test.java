package org.artrev.workshop.fp.part1.practice1;

import io.teoria.junit.numbers.Above;
import io.teoria.junit.numbers.Below;
import io.teoria.junit.numbers.Between;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.experimental.theories.suppliers.TestedOn;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

@RunWith(Theories.class)
public class Part8Test {
    @Theory
    public void values_between_0_and_6_are_allowed(
            @Between(first = 1, last = 5) final Integer value
    ) {

        assertTrue(UsingPredicates.isAllowed.test(value));
    }

    @Theory
    public void value_42_is_allowed(
            @TestedOn(ints = {42}) final Integer value
    ) {
        assertTrue(UsingPredicates.isAllowed.test(value));
    }

    @Theory
    public void values_0_and_6_are_not_allowed(
            @TestedOn(ints = {0, 6}) final Integer value
    ) {
        assertFalse(UsingPredicates.isAllowed.test(value));
    }

    @Theory
    public void values_below_0_are_not_allowed(
            @Below(value = 0) final Integer value
    ) {
        assertFalse(UsingPredicates.isAllowed.test(value));
    }

    @Theory
    public void values_above_6_are_not_allowed(
            @Above(value = 0) final Integer value
    ) {
        assertFalse(UsingPredicates.isAllowed.test(value));
    }
}
