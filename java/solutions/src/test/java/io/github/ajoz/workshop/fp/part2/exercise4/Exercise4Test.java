package io.github.ajoz.workshop.fp.part2.exercise4;

import org.junit.Test;

import java.util.List;

import static io.github.ajoz.workshop.fp.part2.exercise4.Exercise4.*;
import static java.util.Arrays.asList;
import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

public class Exercise4Test {
    @Test
    public void shouldNotContainAValueOnAnEmptyList() {
        // given:
        final List<Integer> emptyList = emptyList();
        final Boolean expected = false;
        // when:
        final Boolean actual = contains(emptyList, 1);
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldNotContainAValue() {
        // given:
        final List<Integer> list = asList(2, 3, 4);
        final Boolean expected = false;
        // when:
        final Boolean actual = contains(list, 1);
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldContainAValue() {
        // given:
        final List<String> stringList = asList("test", "of", "a", "function");
        final Boolean expected = true;
        // when:
        final Boolean actual = contains(stringList, "test");
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldCalculateAverage() {
        // given:
        final List<Integer> list = asList(1, 2, 3);
        final Integer expected = 2;
        // when:
        final Integer actual = average(list);
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldFindLastElement() {
        // given:
        final List<Integer> list = asList(0, 1, 2, 3, 4, 5, 6);
        final Integer expected = 6;
        // when:
        final Integer actual = last(list);
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldJoinAList() {
        // given:
        final List<Boolean> list = asList(true, false, false, true);
        final String expected = "true:false:false:true";
        // when:
        final String actual = join(list, ":");
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldJoinAnEmptyList() {
        final List<Boolean> list = emptyList();
        final String expected = "";
        // when:
        final String actual = join(list, "::");
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldJoinOneElementList() {
        // given:
        final List<Integer> list = singletonList(1);
        final String expected = "1";
        // when:
        final String actual = join(list, "_");
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldCountListElements() {
        // given:
        final List<Integer> list = asList(1, 2, 5, 6);
        final Integer expected = 4;
        // when:
        final Integer actual = count(list);
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldCountEmptyList() {
        // given:
        final List<String> list = emptyList();
        final Integer expected = 0;
        // when:
        final Integer actual = count(list);
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldReverseAnEmptyList() {
        // given:
        final List<String> list = emptyList();
        // when:
        final List<String> actual = reverse(list);
        // then:
        assertTrue(actual.isEmpty());
    }

    @Test
    public void shouldReverseAOneElementList() {
        // given:
        final List<String> list = singletonList("Test");
        final List<String> expected = singletonList("Test");
        // when:
        final List<String> actual = reverse(list);
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldReverseAList() {
        // given:
        final List<Integer> list = asList(1, 2, 3, 4);
        final List<Integer> expected = asList(4, 3, 2, 1);
        // when:
        final List<Integer> actual = reverse(list);
        // then:
        assertThat(actual, is(expected));
    }

    @Test
    public void shouldFindMaximum() {
        // given:
        final List<Integer> list = asList(1, 2, 3, 4);
        final Integer expected = 4;

        // when:
        final Integer actual = maximum(list);

        // then:
        assertEquals(expected, actual);
    }

    @Test
    public void shouldFindMinimum() {
        // given:
        final List<Integer> list = asList(1, 2, 3, 4);
        final Integer expected = 1;

        // when:
        final Integer actual = minimum(list);

        // then:
        assertEquals(expected, actual);
    }

    @Test
    public void shouldFindPenultimateElement() {
        // given:
        final List<Integer> list = asList(1, 2, 3, 4);
        final Integer expected = 3;

        // when:
        final Integer actual = penultimate(list);

        // then:
        assertEquals(expected, actual);
    }
}
