package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_5;

import io.github.ajoz.workshop.fp.java.tools.Function1;

@SuppressWarnings("unused")
public class Exercise5 {
    // Part 1:
    public static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                       final A value) {
        return function.apply(value); // trivial
    }

    // Part 2:
    public static <A, B, C> Function1<A, C> applySecond(final Function1<A, Function1<B, C>> function,
                                                        final B value) {
        return a -> function.apply(a).apply(value);
    }

    // Part 3:
    public static void main(final String[] args) {
        final Function1<Integer, Function1<Integer, Integer>> addInts =
                a -> b -> a + b;

        final Function1<Integer, Integer> addOne =
                addInts.apply(1);

        System.out.println(addOne.apply(0));
        System.out.println(addOne.apply(1));
        System.out.println(addOne.apply(41));

        final Function1<String, Function1<String, String>> concatStrings =
                first -> second -> first + second;

        final Function1<String, String> fooPrefix =
                applyFirst(concatStrings, "foo");

        final Function1<String, String> barSuffix =
                applySecond(concatStrings, "bar");

        System.out.println(fooPrefix.apply("rever with JUG Łódź!"));
        System.out.println(barSuffix.apply("Unfortunately no sponsors for an open "));
    }
}

@SuppressWarnings("unused")
interface Function2<A, B, C> {
    C apply(A a, B b);

    // Part 4:
    default Function1<B, C> applyFirst(final A value) {
        return b -> this.apply(value, b);
    }

    // Part 5:
    default Function1<A, C> applySecond(final B value) {
        return a -> this.apply(a, value);
    }

    // Part 6:
    static void main(final String[] args) {
        final Function2<String, Integer, String> drop =
                (string, amount) -> {
                    final int length = string.length();
                    if (amount < length)
                        return string.substring(amount, length);
                    else
                        return "";
                };

        final Function1<String, Integer> length = String::length;

        final Function1<String, Integer> substrlen =
                drop.applySecond(6).andThen(length);
    }
}
