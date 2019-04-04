package org.artrev.workshop.fp.part1.practice3;

import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

@RunWith(Enclosed.class)
public class ExercisesWithAListTest {
    public static class AllAbove42 {
        @Test
        public void returns_empty_List_for_empty_List() {
            // given:
            final List<Integer> values = Collections.emptyList();

            // when:
            final List<Integer> actual = ExercisesWithAList.allAbove42(values);

            // then:
            assertTrue(actual.isEmpty());
        }

        @Test
        public void returns_empty_List_if_ALL_values_are_below_42() {
            // given:
            final List<Integer> values = Arrays.asList(1, 2, 3);

            // when:
            final List<Integer> actual = ExercisesWithAList.allAbove42(values);

            // then:
            assertTrue(actual.isEmpty());
        }

        @Test
        public void returns_values_above_42() {
            // given:
            final List<Integer> values = Arrays.asList(1, 2, 3, 256);
            final Integer expected = 256;

            // when:
            final List<Integer> actual = ExercisesWithAList.allAbove42(values);

            // then:
            assertEquals(1, actual.size());
            assertEquals(expected, actual.get(0));
        }

        @Test
        public void returns_list_with_the_same_elements_if_all_values_are_above_42() {
            // given:
            final List<Integer> values = Arrays.asList(64, 128, 256, 512);

            // when:
            final List<Integer> actual = ExercisesWithAList.allAbove42(values);

            // then:
            assertEquals(values, actual);
        }
    }

    public static class AllBelow42 {
        @Test
        public void returns_empty_List_for_empty_List() {
            // given:
            final List<Integer> values = Collections.emptyList();

            // when:
            final List<Integer> actual =
                    ExercisesWithAList.allBelow42.apply(values);

            // then:
            assertTrue(actual.isEmpty());
        }

        @Test
        public void returns_empty_List_if_ALL_values_are_above_42() {
            // given:
            final List<Integer> values = Arrays.asList(64, 128, 256);

            // when:
            final List<Integer> actual =
                    ExercisesWithAList.allBelow42.apply(values);

            // then:
            assertTrue(actual.isEmpty());
        }

        @Test
        public void returns_values_below_42() {
            // given:
            final List<Integer> values = Arrays.asList(1, 64, 128, 256);
            final Integer expected = 1;

            // when:
            final List<Integer> actual =
                    ExercisesWithAList.allBelow42.apply(values);

            // then:
            assertEquals(1, actual.size());
            assertEquals(expected, actual.get(0));
        }

        @Test
        public void returns_list_with_the_same_elements_if_all_values_are_below_42() {
            // given:
            final List<Integer> values = Arrays.asList(1, 2, 3);

            // when:
            final List<Integer> actual =
                    ExercisesWithAList.allBelow42.apply(values);

            // then:
            assertEquals(values, actual);
        }
    }
}
