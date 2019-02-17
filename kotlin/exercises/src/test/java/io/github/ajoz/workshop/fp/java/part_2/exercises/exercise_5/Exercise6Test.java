package io.github.ajoz.workshop.fp.java.part_2.exercises.exercise_5;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static io.github.ajoz.workshop.fp.java.part_2.exercises.exercise_5.Exercise5.*;
import static org.junit.Assert.assertEquals;

public class Exercise6Test {
    @Test
    public void should_map_ints() {
        // given:
        final List<Integer> ints = Arrays.asList(1, 2, 3);
        final List<Integer> expected = Arrays.asList(2, 3, 4);
        final Function1<Integer, Integer> addOne = x -> x + 1;

        // when:
        final List<Integer> actual = mapInts(ints, addOne);

        // then:
        assertEquals(expected, actual);
    }

    @Test
    public void should_map_a_list() {
        // given:
        final List<Integer> ints = Arrays.asList(1, 2, 3);
        final List<String> expected = Arrays.asList("1", "2", "3");
        final Function1<Integer, String> toString = x -> "" + x;

        // when:
        final List<String> actual = map(ints, toString);

        // then:
        assertEquals(expected, actual);
    }

    @Test
    public void test_addOne() {
        // given:
        final List<Integer> ints = Arrays.asList(1, 2, 3);
        final List<Integer> expected = Arrays.asList(2, 3, 4);

        // when:
        final List<Integer> actual = addOne(ints);

        // then:
        assertEquals(expected, actual);
    }

    @Test
    public void test_lengths() {
        // given:
        final List<String> strings = Arrays.asList("JUG", "Lodz");
        final List<Integer> expected = Arrays.asList(3, 4);

        // when:
        final List<Integer> actual = lengths(strings);

        // then:
        assertEquals(expected, actual);
    }
}
