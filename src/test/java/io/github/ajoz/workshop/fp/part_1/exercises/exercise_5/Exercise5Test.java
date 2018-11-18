package io.github.ajoz.workshop.fp.part_1.exercises.exercise_5;

import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_5.Exercise5.composeConsumer;
import static io.github.ajoz.workshop.fp.part_1.exercises.exercise_5.Exercise5.composeSupplier;
import org.junit.Assert;
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
        Assert.assertEquals(new Integer(42), tested.get());
    }
}
