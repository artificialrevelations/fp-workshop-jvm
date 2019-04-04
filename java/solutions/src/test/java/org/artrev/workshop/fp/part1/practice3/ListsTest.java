package org.artrev.workshop.fp.part1.practice3;

import org.artrev.workshop.fp.tools.Predicate;
import org.artrev.workshop.fp.tools.predicates.Predicates;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

@RunWith(Enclosed.class)
public class ListsTest {
    public static class Select {
        @Test
        public void should_return_empty_List_for_empty_List() {
            // given:
            final List<String> values = Collections.emptyList();

            // when:
            final List<String> actual =
                    Lists.select(Predicates.alwaysTrue(), values);

            // then:
            assertTrue(actual.isEmpty());
        }

        @Test
        public void should_return_empty_List_if_no_element_satisfies_Predicate() {
            // given:
            final List<String> values = Arrays.asList("JUG", "Lodz", "Workshop");

            // when:
            final List<String> actual =
                    Lists.select(Predicates.alwaysFalse(), values);

            // then:
            assertTrue(actual.isEmpty());
        }

        @Test
        public void should_return_only_elements_that_satisfy_the_Predicate() {
            // given:
            final List<String> values = Arrays.asList("JUG", "Lodz", "Workshop");
            final Predicate<String> predicate = str -> str.length() == 3;
            final String expected = "JUG";

            // when:
            final List<String> actual = Lists.select(predicate, values);

            // then:
            assertEquals(1, actual.size());
            assertEquals(expected, actual.get(0));
        }

        @Test
        public void shouldReturnAllElementsIfAllSatisfyThePredicate() {
            // given:
            final List<String> values = Arrays.asList("JUG", "Lodz", "Workshop");
            final Predicate<String> predicate = Predicates.alwaysTrue();

            // when:
            final List<String> actual = Lists.select(predicate, values);

            // then:
            assertEquals(values, actual);
        }
    }

    public static class Reject {
        @Test
        public void should_return_empty_List_for_empty_List() {
            // given:
            final List<String> values = Collections.emptyList();

            // when:
            final List<String> actual =
                    Lists.select(Predicates.alwaysTrue(), values);

            // then:
            assertTrue(actual.isEmpty());
        }

        @Test
        public void should_return_empty_List_if_ALL_elements_satisfy_the_Predicate() {
            // given:
            final List<String> values = Arrays.asList("JUG", "Lodz", "Workshop");
            final Predicate<String> predicate = str -> str.length() > 2;

            // when:
            final List<String> actual = Lists.reject(predicate, values);

            // then:
            assertTrue(actual.isEmpty());
        }

        @Test
        public void shouldReturnAllElementsThatDoNotSatisfyThePredicate() {
            // given:
            final List<String> values = Arrays.asList("JUG", "Lodz", "Workshop");
            final Predicate<String> predicate = str -> str.length() > 3;
            final String expected = "JUG";

            // when:
            final List<String> actual = Lists.reject(predicate, values);

            // then:
            assertEquals(1, actual.size());
            assertEquals(expected, actual.get(0));
        }

        @Test
        public void shouldReturnAllElementsIfAllDoNotSatisfyThePredicate() {
            // given:
            final List<String> values = Arrays.asList("JUG", "Lodz", "Workshop");

            // when:
            final List<String> actual = Lists.reject(Predicates.alwaysFalse(), values);

            // then:
            assertEquals(values, actual);
        }
    }
}
