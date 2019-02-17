package io.github.ajoz.workshop.fp.java.part1.practice_2;

import io.github.ajoz.workshop.fp.java.tools.Predicate;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

@RunWith(Enclosed.class)
public class PredicatesTest {
    public static class Part1 {
        @Test
        public void is_not_an_instance_of() {
            // given:
            final Predicate<String> tested = Predicates.instanceOf(Integer.class);

            // when:
            final boolean actual = tested.test("JUG Lodz!");

            // then:
            assertFalse(actual);
        }

        @Test
        public void is_an_instance_of() {
            // given:
            final Predicate<Integer> tested = Predicates.instanceOf(Integer.class);

            // when:
            final boolean actual = tested.test(42);

            // then:
            assertTrue(actual);
        }
    }

    public static class Part2 {
        @Test
        public void is_null() {
            // given:
            final Predicate<String> tested = Predicates.isNull();

            // when:
            final boolean actual = tested.test(null);

            // then:
            assertTrue(actual);
        }

        @Test
        public void is_not_null() {
            // given:
            final Predicate<String> tested = Predicates.isNull();

            // when:
            final boolean actual = tested.test("mobilization.pl");

            // then:
            assertFalse(actual);
        }
    }

    public static class Part3 {
        @Test
        public void is_null() {
            // given:
            final Predicate<String> tested = Predicates.isNotNull();

            // when:
            final boolean actual = tested.test(null);

            // then:
            assertFalse(actual);
        }

        @Test
        public void is_not_null() {
            // given:
            final Predicate<String> tested = Predicates.isNotNull();

            // when:
            final boolean actual = tested.test("check JUG Lodz meetup!");

            // then:
            assertTrue(actual);
        }
    }

    public static class Part4 {
        @Test
        public void is_always_True() {
            // given:
            final Predicate<String> tested = Predicates.alwaysTrue();

            // I know, I know -- single assert per test ;-)
            assertTrue(tested.test(null));
            assertTrue(tested.test(""));
            assertTrue(tested.test(" "));
            assertTrue(tested.test("    "));
            assertTrue(tested.test("check JUG Lodz facebook!"));
        }
    }

    public static class Part5 {
        @Test
        public void is_always_False() {
            // given:
            final Predicate<String> tested = Predicates.alwaysFalse();

            // the same as above :>
            assertFalse(tested.test(null));
            assertFalse(tested.test(""));
            assertFalse(tested.test(" "));
            assertFalse(tested.test("    "));
            assertFalse(tested.test("check JUG Lodz facebook!"));
        }
    }

    public static class Part6 {
        @Test
        public void is_equal_to() {
            // given:
            final Predicate<String> tested = Predicates.isEqualTo("foo");

            // when:
            final boolean actual = tested.test("foo");

            // then:
            assertTrue(actual);
        }

        @Test
        public void is_not_equal_to() {
            // given:
            final Predicate<String> tested = Predicates.isEqualTo("foo");

            // when:
            final boolean actual = tested.test("bar");

            // then:
            assertFalse(actual);
        }
    }
}
