package io.github.ajoz.workshop.fp.part_1.solutions.exercise_5;

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);
}

@FunctionalInterface
interface Consumer1<A> {
    void accept(A a);
}

@FunctionalInterface
interface Supplier<A> {
    A get();
}

class Exercise5 {
    static <A, B> Consumer1<A> composeConsumer(final Function1<A, B> function, final Consumer1<B> consumer) {
        return (A a) -> consumer.accept(function.apply(a));
    }

    static <A, B> Supplier<B> composeSupplier(final Function1<A, B> function, final Supplier<A> supplier) {
        return () -> function.apply(supplier.get());
    }

    static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                final Supplier<A> supplier) {
        return (B b) -> function.apply(supplier.get()).apply(b);
    }
}