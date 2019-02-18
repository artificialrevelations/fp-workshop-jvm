package io.github.ajoz.workshop.fp.java.part1.exercise5;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

import static io.github.ajoz.workshop.fp.java.part1.exercise5.Exercise5.applyFirst;
import static io.github.ajoz.workshop.fp.java.part1.exercise5.Exercise5.applySecond;
import static org.junit.Assert.assertEquals;

@RunWith(Enclosed.class)
public class Exercise5Test {
    public static final class Part1 {
        @Test
        public void should_apply_first_argument() {
            // given:
            final Integer first = 1;
            final Integer second = 1;
            final Integer expected = 2;

            final Function1<Integer, Function1<Integer, Integer>> curried =
                    a -> b -> a + b;

            final Function1<Integer, Integer> tested =
                    applyFirst(curried, first);

            // when:
            final Integer actual = tested.apply(second);

            // then:
            assertEquals(expected, actual);
        }
    }

    public static final class Part2 {
        @Test
        public void should_apply_second_argument() {
            // given:
            final String first = "foobar";
            final Integer second = 42;
            final String expected = "foobar42";

            final Function1<String, Function1<Integer, String>> curried =
                    string -> integer -> string + integer;

            final Function1<String, String> tested =
                    applySecond(curried, second);

            // when:
            final String actual = tested.apply(first);

            // then:
            assertEquals(expected, actual);
        }
    }

    public static final class Part4 {
        @Test
        public void should_apply_first_argument() {
            // given:
            final Integer first = 1;
            final Integer second = 1;
            final Integer expected = 2;

            final Function2<Integer, Integer, Integer> add =
                    (a, b) -> a + b;

            final Function1<Integer, Integer> tested =
                    add.applyFirst(first);

            // when:
            final Integer actual = tested.apply(second);

            // then:
            assertEquals(expected, actual);
        }
    }

    public static final class Part5 {
        @Test
        public void should_apply_second_argument() {
            // given:
            final String first = "foobar";
            final Integer second = 42;
            final String expected = "foobar42";

            final Function2<String, Integer, String> concat =
                    (string, integer) -> string + integer;

            final Function1<String, String> tested =
                    concat.applySecond(second);

            // when:
            final String actual = tested.apply(first);

            // then:
            assertEquals(expected, actual);
        }
    }
}
