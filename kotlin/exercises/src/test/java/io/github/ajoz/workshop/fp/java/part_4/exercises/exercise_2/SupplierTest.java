package io.github.ajoz.workshop.fp.java.part_4.exercises.exercise_2;


import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import java.util.Random;

import static org.junit.Assert.assertEquals;

@RunWith(Enclosed.class)
public class SupplierTest {
    public static class Memoized {
        @Test
        public void should_supply_a_new_value_on_first_run() {
            // given:
            final Integer expected = 42;
            final Supplier<Integer> supplier = () -> expected;

            // when:
            final Supplier<Integer> tested = supplier.memoized();
            final Integer actual = tested.get();

            // then:
            assertEquals(expected, actual);
        }

        @Test
        public void should_supply_the_same_value_on_each_call() {
            // given:
            final Supplier<Boolean> supplier = () -> new Random().nextBoolean();

            // when:
            final Supplier<Boolean> tested = supplier.memoized();

            // initial run (first):
            final Boolean expected = tested.get();

            // consecutive runs:
            final Boolean second = tested.get();
            final Boolean third = tested.get();

            // then:
            assertEquals(expected, second);
            assertEquals(expected, third);
        }
    }

    public static class Memoize {
        @Test
        public void should_supply_a_new_value_on_first_run() {
            // given:
            final Integer expected = 42;
            final Supplier<Integer> tested = Supplier.memoize(() -> expected);

            // when:
            final Integer actual = tested.get();

            // then:
            assertEquals(expected, actual);
        }

        @Test
        public void should_supply_the_same_value_on_each_call() {
            // given:
            final Supplier<Boolean> tested = Supplier.memoize(() -> new Random().nextBoolean());

            // when:
            // initial run (first):
            final Boolean expected = tested.get();

            // consecutive runs:
            final Boolean second = tested.get();
            final Boolean third = tested.get();

            // then:
            assertEquals(expected, second);
            assertEquals(expected, third);
        }
    }
}