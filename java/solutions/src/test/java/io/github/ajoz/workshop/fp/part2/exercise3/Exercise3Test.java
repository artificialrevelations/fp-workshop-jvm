package io.github.ajoz.workshop.fp.part2.exercise3;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static io.github.ajoz.workshop.fp.part2.exercise3.Exercise3.*;
import static org.junit.Assert.assertEquals;

public class Exercise3Test {
    @Test
    public void sum_with_foldRight_should_be_equal_to_sum_with_foldLeft() {
        // given:
        final List<Integer> list = Arrays.asList(1, 2, 3);

        // when:
        final Integer sumLeft = foldLeft(list, 0, (a, b) -> a + b);
        final Integer sumRight = foldRight(list, 0, (a, b) -> a + b);

        // then:
        assertEquals(sumLeft, sumRight);
    }

    @Test
    public void foldRight_structure_should_be_correct() {
        // given:
        final List<Integer> list = Arrays.asList(1, 2, 3);
        final String expected = "(1 + (2 + (3 + 0)))";

        // when:
        final String actual = foldRight(list, "0",
                (integer, string) -> String.format("(%d + %s)", integer, string)
        );

        // then:
        assertEquals(expected, actual);
    }

    @Test
    public void foldRight_in_terms_of_foldLeft() {
        // given:
        final List<Integer> list = Arrays.asList(1, 2, 3);
        final String expected = "(1 + (2 + (3 + 0)))";

        // when:
        final String actual = foldRight2(list, "0",
                (integer, string) -> String.format("(%d + %s)", integer, string)
        );

        // then:
        assertEquals(expected, actual);
    }
}
