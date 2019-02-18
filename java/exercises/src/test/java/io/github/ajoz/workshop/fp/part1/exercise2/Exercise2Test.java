package io.github.ajoz.workshop.fp.part1.exercise2;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertEquals;

@RunWith(Enclosed.class)
public class Exercise2Test {
    public static final class Part1 {
        @Test
        public void test_add_one() {
            // given:
            final Integer value = 41;
            final Integer expected = 42;

            // when:
            final Integer actual =
                    Exercise2.f1.apply(value);

            // then:
            assertEquals(expected, actual);
        }

        @Test
        public void test_string_length() {
            // given:
            final String value = "JUG Lodz!";
            final Integer expected = value.length();

            // when:
            final Integer actual =
                    Exercise2.f2.apply(value);

            // then:
            assertEquals(expected, actual);
        }

        @Test
        public void test_add_foo() {
            // given:
            final String value = "bar";
            final String expected = "foobar";

            // when:
            final String actual =
                    Exercise2.f3.apply(value);

            // then:
            assertEquals(expected, actual);
        }
    }

/*
    public static final class Part2 {
        @Test
        public void test_str2int() {
            // given:
            final String value = "42";
            final Integer expected = 42;

            // when:
            final Integer actual =
                    Exercise2.str2int.apply(value);

            // then:
            assertEquals(expected, actual);
        }

        @Test
        public void number_0_is_not_42() {
            // given:
            final Integer value = 0;

            // when:
            final Boolean actual =
                    Exercise2.int2bool.apply(value);

            // then:
            assertFalse(actual);
        }

        @Test
        public void number_42_is_42() {
            // given:
            final Integer value = 42;

            // when:
            final Boolean actual =
                    Exercise2.int2bool.apply(value);

            // then:
            assertTrue(actual);
        }

        @Test
        public void string_42_is_number_42_after_conversion() {
            // given:
            final String value = "42";

            // when:
            final Boolean actual =
                    Exercise2.str2bool.apply(value);

            // then:
            assertTrue(actual);
        }

        @Test
        public void string_24_is_not_a_number_42_after_conversion() {
            // given:
            final String value = "24";

            // when:
            final Boolean actual =
                    Exercise2.str2bool.apply(value);

            // then:
            assertFalse(actual);
        }
    }

    public final static class Part3 {
        @Test
        public void identity_for_String() {
            // given:
            final String value = "JUG Lodz!";
            final String expected = "JUG Lodz!";

            // when:
            final String actual =
                    Exercise2.<String>identity().apply(value);

            // then:
            assertEquals(expected, actual);
        }

        @Test
        public void identity_for_Integer() {
            // given:
            final Integer value = 42;
            final Integer expected = 42;

            // when:
            final Integer actual =
                    Exercise2.<Integer>identity().apply(value);

            // then:
            assertEquals(expected, actual);
        }
    }

    public static final class Part4 {
        @Test
        public void constant_String_to_String() {
            // given:
            final String value = "Mobilization.pl";
            final String constant = "JUG Lodz!";
            final String expected = "JUG Lodz!";

            // when:
            final String actual =
                    Exercise2.<String, String>constant(constant).apply(value);

            // then:
            assertEquals(expected, actual);
        }

        @Test
        public void constant_String_to_Integer() {
            // given:
            final String value = "Mobilization.pl";
            final Integer constant = 42;
            final Integer expected = 42;

            // when:
            final Integer actual =
                    Exercise2.<String, Integer>constant(constant).apply(value);

            // then:
            assertEquals(expected, actual);
        }
    }

    */
}
