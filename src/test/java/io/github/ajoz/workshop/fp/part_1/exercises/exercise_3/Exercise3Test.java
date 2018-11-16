package io.github.ajoz.workshop.fp.part_1.exercises.exercise_3;

import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_3.Exercise3.compose;
import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_3.Exercise3.composeAll;
import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_3.Exercise3.composeIntFuns;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;

@RunWith(Enclosed.class)
public class Exercise3Test {
    public static class ComposeIntFuns {
        @Test
        public void should_compose_two_simple_functions() {
            // given:
            final Function1<Integer, Integer> f = x -> x + 1;
            final Function1<Integer, Integer> g = x -> x + 2;

            // when:
            final Function1<Integer, Integer> h = composeIntFuns(f, g);

            // then:
            assertEquals(new Integer(3), h.apply(0));
            assertEquals(new Integer(4), h.apply(1));
        }

        @Test
        public void should_compose_two_identity_functions() {
            // given:
            final Function1<Integer, Integer> f = x -> x;
            final Function1<Integer, Integer> g = x -> x;

            // when:
            final Function1<Integer, Integer> h = composeIntFuns(f, g);

            // then:
            assertEquals(new Integer(0), h.apply(0));
            assertEquals(new Integer(1), h.apply(1));
        }

        @Test
        public void should_compose_two_isomorphic_functions() {
            // given:
            final Function1<Integer, Integer> f = x -> x + 1;
            final Function1<Integer, Integer> g = x -> x - 1;

            // when:
            final Function1<Integer, Integer> h = composeIntFuns(f, g);

            // then:
            assertEquals(new Integer(0), h.apply(0));
            assertEquals(new Integer(1), h.apply(1));
        }
    }

    public static class Compose {
        @Test
        public void should_compose_two_functions() {
            // given:
            final Function1<String, Integer> f = Integer::valueOf;
            final Function1<Integer, Boolean> g = integer -> integer == 42;

            // when:
            final Function1<String, Boolean> h = compose(f, g);

            // then:
            assertEquals(Boolean.TRUE, h.apply("42"));
            assertEquals(Boolean.FALSE, h.apply("24"));
        }
    }

    public static class ComposeAll {
        @Test
        public void multiple_functions() {
            // given:
            final Function1<Integer, Integer> f = x -> x + 1;
            final Function1<Integer, Integer> g = x -> x + 2;
            final Function1<Integer, Integer> h = x -> x + 3;
            final Function1<Integer, Integer> i = x -> x + 4;

            // when:
            final Function1<Integer, Integer> j = composeAll(
                    f, g, h, i
            );

            // then:
            assertEquals(new Integer(10), j.apply(0));
            assertEquals(new Integer(0), j.apply(-10));
        }
    }

    public static class Function1Interface {
        @Test
        public void should_use_andThen() {
            // given:
            final Function1<String, Integer> f = Integer::valueOf;
            final Function1<Integer, Boolean> g = integer -> integer == 42;

            // when:
            final Function1<String, Boolean> h = f.andThen(g);

            // then:
            assertEquals(Boolean.TRUE, h.apply("42"));
            assertEquals(Boolean.FALSE, h.apply("24"));
        }

        @Test
        public void should_use_compose() {
            // given:
            final Function1<Integer, Boolean> g = integer -> integer == 42;
            final Function1<String, Integer> f = Integer::valueOf;

            // when:
            final Function1<String, Boolean> h = g.compose(f);

            // then:
            assertEquals(Boolean.TRUE, h.apply("42"));
            assertEquals(Boolean.FALSE, h.apply("24"));
        }
    }
}
