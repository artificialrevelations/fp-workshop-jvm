package io.github.ajoz.workshop.fp.part_1.solutions.exercise_5;

import static io.github.ajoz.workshop.fp.part_1.solutions.exercise_5.Exercise5.applyFirst;
import static io.github.ajoz.workshop.fp.part_1.solutions.exercise_5.Exercise5.composeConsumer;
import static io.github.ajoz.workshop.fp.part_1.solutions.exercise_5.Exercise5.composeSupplier;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class Exercise5Test {
    @Test
    public void should_compose_function_with_consumer() {
        // given:
        final Consumer1<Integer> consumer =
                integer -> System.out.println(String.format("consumed: %d", integer));

        final Function1<String, Integer> function = Integer::valueOf;

        // when:
        final Consumer1<String> tested = composeConsumer(function, consumer);

        // then:
        tested.accept("42");
    }

    @Test
    public void should_compose_function_with_supplier() {
        // given:
        final Supplier<String> supplier =
                () -> "42";

        final Function1<String, Integer> function = Integer::valueOf;

        // when:
        final Supplier<Integer> tested = composeSupplier(function, supplier);

        // then:
        assertEquals(new Integer(42), tested.get());
    }

    @Test
    public void should_apply_first_argument() {
        // given:
        final Supplier<String> supplier =
                () -> "foo";

        final Function1<String, Function1<Integer, String>> function =
                string -> integer -> String.format("%s : %d", string, integer);

        // when:
        final Function1<Integer, String> tested = applyFirst(function, supplier);

        // then:
        assertEquals(function.apply("foo").apply(42), tested.apply(42));
    }
}
