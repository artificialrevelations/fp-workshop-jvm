package io.github.ajoz.workshop.fp.java.tools;

import kotlin.Pair;

@FunctionalInterface
public interface Function2<A, B, C> {
    C apply(A a, B b);

    default <D> Function2<A, B, D> andThen(final Function1<C, D> after) {
        return (A a, B b) -> after.apply(this.apply(a, b));
    }

    default <D> Function2<D, B, C> composeFirst(final Function1<D, A> before) {
        return (D d, B b) -> this.apply(before.apply(d), b);
    }

    default <D> Function2<A, D, C> composeSecond(final Function1<D, B> before) {
        return (A a, D d) -> this.apply(a, before.apply(d));
    }

    default Function2<B, A, C> flip() {
        return (B b, A a) -> this.apply(a, b);
    }

    default Function1<A, Function1<B, C>> curry() {
        return (A a) -> (B b) -> this.apply(a, b);
    }

    default Function1<Pair<A, B>, C> tupled() {
        return (Pair<A, B> pair) -> this.apply(pair.getFirst(), pair.getSecond());
    }

    default Function1<B, C> applyFirst(final Supplier<A> supplier) {
        return (B b) -> this.apply(supplier.get(), b);
    }

    default Function1<A, C> applySecond(final Supplier<B> supplier) {
        return (A a) -> this.apply(a, supplier.get());
    }

    default Function2<A, B, C> memoized() {
//        final ConcurrentHashMap<Pair<A, B>, C> memo = new ConcurrentHashMap<>();
//        return (A a, B b) -> {
//            final Pair<A, B> key = new Pair<>(a, b);
//            if (!memo.containsKey(key)) {
//                memo.put(key, this.apply(a, b));
//            }
//            return memo.get(key);
//        };
        // maybe a bit too much composition, nah! ;-)
        return untuple(tupled().memoized());
    }

    static <A, B, C> Function2<A, B, C> uncurry(final Function1<A, Function1<B, C>> function1) {
        return (A a, B b) -> function1.apply(a).apply(b);
    }

    static <A, B, C> Function2<A, B, C> untuple(final Function1<Pair<A, B>, C> function) {
        return (a, b) -> function.apply(new Pair<>(a, b));
    }
}
