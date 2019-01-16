package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_2;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
            assertFalse(actual);
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
}
