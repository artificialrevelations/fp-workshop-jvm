package org.artrev.workshop.fp.part1.exercise6;

import org.artrev.workshop.fp.tools.Function1;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class Exercise6Test {
    @Test
    public void should_compose_function_with_consumer() {
        // given:
        final Consumer1<Integer> consumer =
                integer -> System.out.println(String.format("consumed: %d", integer));

        final Function1<String, Integer> function =
                Integer::valueOf;

        // when:
        final Consumer1<String> tested =
                Exercise6.composeConsumer(function, consumer);

        // then:
        tested.accept("42");
    }

    @Test
    public void should_compose_function_with_supplier() {
        // given:
        final Supplier<String> supplier = () -> "42";
        final Function1<String, Integer> function =
                Integer::valueOf;

        // when:
        final Supplier<Integer> tested =
                Exercise6.composeSupplier(supplier, function);

        // then:
        assertEquals(new Integer(42), tested.get());
    }

    @Test
    public void should_apply_first_argument() {
        // given:
        final Supplier<String> supplier = () -> "foo";
        final Function1<String, Function1<Integer, String>> function =
                string -> integer -> String.format("%s : %d", string, integer);

        // when:
        final Function1<Integer, String> tested =
                Exercise6.applyFirst(function, supplier);

        // then:
        assertEquals(function.apply("foo").apply(42), tested.apply(42));
    }

    @Test
    public void should_apply_second_argument() {
        // given:
        final Supplier<Integer> supplier = () -> 42;
        final Function1<String, Function1<Integer, String>> function =
                string -> integer -> String.format("%s : %d", string, integer);

        // when:
        final Function1<String, String> tested =
                Exercise6.applySecond(function, supplier);

        // then
        assertEquals(function.apply("foo").apply(42), tested.apply("foo"));
    }
}
