package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_3;

class Exercise3 {
    static Function1<Integer, Integer> composeIntFuns(final Function1<Integer, Integer> first,
                                                      final Function1<Integer, Integer> second) {
        return (Integer value) -> second.apply(first.apply(value));
    }

    // hint: Allow the types to guide you with the composition
    static <A, B, C> Function1<A, C> compose(final Function1<A, B> f,
                                             final Function1<B, C> g) {
        return (A a) -> g.apply(f.apply(a));
    }

    @SafeVarargs
    static <A> Function1<A, A> composeAll(final Function1<A, A>... functions) {
        // as an application loop:
//        return (A a) -> {
//            A result = a;
//            for (final Function1<A, A> function : functions) {
//                result = function.apply(result);
//            }
//            return result;
//        };
//
        // using compose and another function:
        Function1<A, A> result = x -> x;
        for (Function1<A, A> function : functions) {
            result = compose(result, function);
        }
        return result;
    }
}

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);

    default <C> Function1<A, C> andThen(final Function1<B, C> after) {
        return a -> after.apply(this.apply(a));
    }

    default <C> Function1<C, B> compose(final Function1<C, A> before) {
        return c -> this.apply(before.apply(c));
    }
}