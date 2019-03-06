package io.github.ajoz.workshop.fp.part1.practice3;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class Practice3Test {
    @Test
    public void returns_empty_List_for_an_empty_List() {
        // given:
        final List<String> values = Collections.emptyList();

        // when:
        final List<String> actual = Practice3.getStrings(values);

        // then:
        assertTrue(actual.isEmpty());
    }

    @Test
    public void returns_empty_List_if_no_strings_are_correct() {
        // given:
        final List<String> values = Arrays.asList(
                null, //one that is null
                "", // one that is empty
                "aJUGa", //one that has JUG but no capital first or last
                "C++++", //one that has a capital first but no JUG
                "c++++J", //one that has a capital last but no JUG
                "JUG", //one that has JUG and first or last capital but is not larger then 3
                "C12345JUG6789D" // one that has JUG, first or last capital but is not smaller then 10
        );

        // when:
        final List<String> actual = Practice3.getStrings(values);

        // then:
        assertTrue(actual.isEmpty());
    }

    @Test
    public void returns_List_with_Strings_that_satisfy_the_predicate() {
        // given:
        final List<String> values = Arrays.asList(
                null,
                "",
                "JUG is NOT cool!!!!",
                "JUG=cool",
                "This workshop sucks",
                "JUG Lodz",
                "C12345JUG6789D",
                "JoinJUG"
        );

        final List<String> expected = Arrays.asList(
                "JUG=cool",
                "JUG Lodz",
                "JoinJUG"
        );

        // when:
        final List<String> actual = Practice3.getStrings(values);

        // then:
        assertEquals(expected, actual);
    }
}
