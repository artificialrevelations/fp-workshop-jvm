package io.github.ajoz.workshop.fp.java.part1.solutions.practice_2;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

@RunWith(Enclosed.class)
public class UsingPredicatesTest {
    public static class Part9 {
        @Test
        public void True_if_string_is_null() {
            // given:
            final String value = null;

            // when:
            final boolean actual = UsingPredicates.isNullOrEmpty.test(value);

            // then:
            assertTrue(actual);
        }

        @Test
        public void False_if_string_is_not_empty() {
            // given:
            final String value = "JUG Lodz";

            // when:
            final boolean actual = UsingPredicates.isNullOrEmpty.test(value);

            // then:
            assertFalse(actual);
        }

        @Test
        public void True_if_string_is_empty() {
            // given:
            final String value = "";

            // when:
            final boolean actual = UsingPredicates.isNullOrEmpty.test(value);

            // then:
            assertTrue(actual);
        }
    }

    public static class Part10 {
        @Test
        public void value_3_is_allowed() {
            assertTrue(UsingPredicates.isAllowed.test(3));
        }

        @Test
        public void value_42_is_allowed() {
            assertTrue(UsingPredicates.isAllowed.test(42));
        }
    }
}
