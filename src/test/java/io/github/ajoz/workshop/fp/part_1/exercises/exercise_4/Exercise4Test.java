package io.github.ajoz.workshop.fp.part_1.exercises.exercise_4;

import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_4.Exercise4.convertToFunction1WithFunction;
import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_4.Exercise4.convertToFunction1WithPair;
import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_4.Exercise4.convertToFunction2FromFunction;
import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_4.Exercise4.convertToFunction2FromPair;
import kotlin.Pair;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class Exercise4Test {
    @Test
    public void should_convert_to_Function1_with_a_tuple() {
        // given:
        final Function2<Integer, Integer, Integer> add = (a, b) -> a + b;
        // when:
        final Function1<Pair<Integer, Integer>, Integer> tested = convertToFunction1WithPair(add);
        // then:
        assertEquals(add.apply(1, 1), tested.apply(new Pair<>(1, 1)));
        assertEquals(add.apply(0, 0), tested.apply(new Pair<>(0, 0)));
        assertEquals(add.apply(-1, 1), tested.apply(new Pair<>(-1, 1)));
    }

    @Test
    public void should_convert_to_Function1_with_a_Function1_return_type() {
        // given:
        final Function2<Integer, Integer, Integer> add = (a, b) -> a + b;
        // when:
        final Function1<Integer, Function1<Integer, Integer>> tested = convertToFunction1WithFunction(add);
        // then:
        assertEquals(add.apply(1, 1), tested.apply(1).apply(1));
        assertEquals(add.apply(0, 0), tested.apply(0).apply(0));
        assertEquals(add.apply(-1, 1), tested.apply(-1).apply(1));
    }

    @Test
    public void should_convert_to_Function2_from_a_Function1_with_a_tuple() {
        // given:
        final Function1<Pair<Integer, Integer>, Integer> add =
                ab_pair -> ab_pair.getFirst() + ab_pair.getSecond();
        // when:
        final Function2<Integer, Integer, Integer> tested = convertToFunction2FromPair(add);
        // then:
        assertEquals(add.apply(new Pair<>(1, 1)), tested.apply(1, 1));
        assertEquals(add.apply(new Pair<>(0, 0)), tested.apply(0, 0));
        assertEquals(add.apply(new Pair<>(-1, 1)), tested.apply(-1, 1));
    }

    @Test
    public void should_convert_to_Function2_from_a_Function1_with_a_Function1_as_return_type() {
        // given:
        final Function1<Integer, Function1<Integer, Integer>> add =
                a -> b -> a + b;
        // when:
        final Function2<Integer, Integer, Integer> tested =
                convertToFunction2FromFunction(add);
        // then:
        assertEquals(add.apply(1).apply(1), tested.apply(1, 1));
        assertEquals(add.apply(0).apply(0), tested.apply(0, 0));
        assertEquals(add.apply(-1).apply(1), tested.apply(-1, 1));
    }
}
