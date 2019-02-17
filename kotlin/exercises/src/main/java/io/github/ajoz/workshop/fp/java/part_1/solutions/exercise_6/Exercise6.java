package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_6;

import io.github.ajoz.workshop.fp.java.tools.Function1;

@FunctionalInterface
interface Consumer1<A> {
    void accept(A a);
}

@FunctionalInterface
interface Supplier<A> {
    A get();
}

class Exercise6 {
    // Part 1:
    static <A, B> Consumer1<A> composeConsumer(final Function1<A, B> function,
                                               final Consumer1<B> consumer) {
        return (A a) -> consumer.accept(function.apply(a));
    }

    // Part 2:
    static final class ComposingConsumer {
        public static void main(final String[] args) {
            final Consumer1<Integer> printInt = System.out::println;
            final Function1<String, Integer> strlen = String::length;

            final Consumer1<String> printStrLen =
                    composeConsumer(strlen, printInt);

            // we could even do:
            final Consumer1<String> printStrLen2 =
                    composeConsumer(String::length, System.out::println);

            printStrLen.accept("https://www.meetup.com/Java-User-Group-Lodz/");
            printStrLen2.accept("https://www.meetup.com/Java-User-Group-Lodz/");
        }
    }

    // Part 3:
    static <A, B> Supplier<B> composeSupplier(final Supplier<A> supplier,
                                              final Function1<A, B> function) {
        return () -> function.apply(supplier.get());
    }

    // Part 4:
    static final class ComposingSupplier {
        public static void main(final String[] args) {
            final Supplier<String> getFacebook = () -> "https://www.facebook.com/groups/juglodz/";
            final Function1<String, Integer> strlen = String::length;

            final Supplier<Integer> getFBLen =
                    composeSupplier(getFacebook, strlen);

            System.out.println(getFBLen.get());
        }
    }

    // Part 5:
    static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                final Supplier<A> supplier) {
        return (B b) -> function.apply(supplier.get()).apply(b);
    }

    // Part 6:
    static <A, B, C> Function1<A, C> applySecond(final Function1<A, Function1<B, C>> function,
                                                 final Supplier<B> supplier) {
        return (A a) -> function.apply(a).apply(supplier.get());
    }
}