package io.github.ajoz.workshop.fp.part1.practice1;

import io.github.ajoz.workshop.fp.tools.Function1;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

@RunWith(Enclosed.class)
public class PredicateTest {
    public static class Part1 {
        @Test
        public void True_and_False_equals_False() {
            // given:
            final Predicate<String> true1 = string -> true;
            final Predicate<String> false1 = string -> false;
            // when:
            final Predicate<String> result = true1.and(false1);
            // then:
            assertFalse(result.test("True_and_False_equals_False"));
        }

        @Test
        public void False_and_True_equals_False() {
            // given:
            final Predicate<String> false1 = string -> false;
            final Predicate<String> true1 = string -> true;
            // when:
            final Predicate<String> result = false1.and(true1);
            // then:
            assertFalse(result.test("False_and_True_equals_False"));
        }

        @Test
        public void False_and_False_equals_False() {
            // given:
            final Predicate<String> false1 = string -> false;
            final Predicate<String> false2 = string -> false;
            // when:
            final Predicate<String> result = false1.and(false2);
            // then:
            assertFalse(result.test("False_and_False_equals_False"));
        }

        @Test
        public void True_and_True_equals_True() {
            // given:
            final Predicate<String> true1 = string -> true;
            final Predicate<String> true2 = string -> true;
            // when:
            final Predicate<String> result = true1.and(true2);
            // then:
            assertTrue(result.test("True_and_True_equals_True"));
        }
    }

    public static class Part2 {
        @Test
        public void True_or_True_equals_True() {
            // given:
            final Predicate<String> true1 = string -> true;
            final Predicate<String> true2 = string -> true;
            // when:
            final Predicate<String> result = true1.or(true2);
            // then:
            assertTrue(result.test("True_or_True_equals_True"));
        }

        @Test
        public void True_or_False_equals_True() {
            // given:
            final Predicate<String> true1 = string -> true;
            final Predicate<String> false1 = string -> false;
            // when:
            final Predicate<String> result = true1.or(false1);
            // then:
            assertTrue(result.test("True_or_False_equals_True"));
        }

        @Test
        public void False_or_True_equals_True() {
            // given:
            final Predicate<String> false1 = string -> false;
            final Predicate<String> true1 = string -> true;
            // when:
            final Predicate<String> result = false1.or(true1);
            // then:
            assertTrue(result.test("False_or_True_equals_True"));
        }

        @Test
        public void False_or_False_equals_False() {
            // given:
            final Predicate<String> false1 = string -> false;
            final Predicate<String> false2 = string -> false;
            // when:
            final Predicate<String> result = false1.or(false2);
            // then:
            assertFalse(result.test("False_or_False_equals_False"));
        }
    }

    public static class Part3 {
        @Test
        public void not_True_equals_False() {
            // given:
            final Predicate<String> aTrue = string -> true;
            // when:
            final Predicate<String> negated = aTrue.not();
            // then:
            assertFalse(negated.test("not_True_equals_False"));
        }

        @Test
        public void not_not_True_equals_True() {
            // given:
            final Predicate<String> aTrue = string -> true;
            // when:
            final Predicate<String> negated = aTrue.not().not();
            // then:
            assertTrue(negated.test("not_not_True_equals_True"));
        }

        @Test
        public void not_False_equals_True() {
            // given:
            final Predicate<String> aFalse = string -> false;
            // when:
            final Predicate<String> negated = aFalse.not();
            // then:
            assertTrue(negated.test("not_False_equals_True"));
        }

        @Test
        public void not_not_False_equals_False() {
            // given:
            final Predicate<String> aFalse = string -> false;
            // when:
            final Predicate<String> negated = aFalse.not().not();
            // then:
            assertFalse(negated.test("not_not_False_equals_False"));
        }
    }

    public static class Part4 {
        @Test
        public void True_xor_True_equals_False() {
            // given:
            final Predicate<String> true1 = string -> true;
            final Predicate<String> true2 = string -> true;
            // when:
            final Predicate<String> result = true1.xor(true2);
            // then:
            assertFalse(result.test("True_xor_True_equals_False"));
        }

        @Test
        public void True_xor_False_equals_True() {
            // given:
            final Predicate<String> true1 = string -> true;
            final Predicate<String> false1 = string -> false;
            // when:
            final Predicate<String> result = true1.xor(false1);
            // then:
            assertTrue(result.test("True_xor_False_equals_True"));
        }

        @Test
        public void False_xor_True_equals_True() {
            // given:
            final Predicate<String> false1 = string -> false;
            final Predicate<String> true1 = string -> true;
            // when:
            final Predicate<String> result = false1.xor(true1);
            // then:
            assertTrue(result.test("False_xor_True_equals_True"));
        }

        @Test
        public void False_xor_False_equals_False() {
            // given:
            final Predicate<String> false1 = string -> false;
            final Predicate<String> false2 = string -> false;
            // when:
            final Predicate<String> result = false1.xor(false2);
            // then:
            assertFalse(result.test("False_xor_False_equals_False"));
        }
    }

    public static class Part5 {
        @Test
        public void should_return_the_same_result_as_a_function() {
            // given:
            final String value = "JUG Lodz!";
            final Predicate<String> predicate = string -> false;

            // when:
            final Function1<String, Boolean> tested = predicate.asFunction1();

            // then:
            assertEquals(predicate.test(value), tested.apply(value));
        }
    }

    public static class Part6 {
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

    public static class Part7 {
        @Test
        public void True_if_string_is_null() {
            // given:
            final String value = null;

            // when:
            final boolean actual = UsingPredicates.isNullOrBlank.test(value);

            // then:
            assertTrue(actual);
        }

        @Test
        public void False_if_string_is_not_empty() {
            // given:
            final String value = "JUG Lodz";

            // when:
            final boolean actual = UsingPredicates.isNullOrBlank.test(value);

            // then:
            assertFalse(actual);
        }

        @Test
        public void True_if_string_is_empty() {
            // given:
            final String value = "";

            // when:
            final boolean actual = UsingPredicates.isNullOrBlank.test(value);

            // then:
            assertTrue(actual);
        }

        @Test
        public void True_if_string_has_only_whitespaces() {
            // given:
            final String value = "      ";

            // when:
            final boolean actual = UsingPredicates.isNullOrBlank.test(value);

            // then:
            assertTrue(actual);
        }
    }
}
