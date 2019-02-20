package io.github.ajoz.workshop.fp.part1.exercise4;

import io.github.ajoz.workshop.fp.tools.Function1;
import io.github.ajoz.workshop.fp.tools.Function2;

// Part 2
class Pair<A, B> {
    final A first;
    final B second;

    Pair(A a, B b) {
        first = a;
        second = b;
    }
}

class Exercise4 {
    static <A, B, C> Function1<Pair<A, B>, C> tuple(final Function2<A, B, C> function2) {
        return tuple -> function2.apply(tuple.first, tuple.second);
    }

    // Part 3
    static <A, B, C> Function2<A, B, C> untuple(final Function1<Pair<A, B>, C> function1) {
        return (A a, B b) -> function1.apply(new Pair<>(a, b));
    }

    // Part 4
    static final class TupleExample {
        static final Function2<String, String, String> addPrefixUT =
                (prefix, text) -> prefix + text;

        static final Function1<Pair<Integer, Integer>, Integer> addIntsT =
                pint -> pint.first + pint.second;

        public static void main(final String[] args) {
            final Function1<Pair<String, String>, String> addPrefixT =
                    tuple(addPrefixUT);

            System.out.println(addPrefixT.apply(new Pair<>("https://", "nozama.com")));

            final Function2<Integer, Integer, Integer> addIntsUT =
                    untuple(addIntsT);

            System.out.println(addIntsUT.apply(42, 0));
        }
    }

    // Part 5
    static <A, B, C> Function1<A, Function1<B, C>> curry(final Function2<A, B, C> function2) {
        return (A a) -> (B b) -> function2.apply(a, b);
    }

    // Part 6
    static <A, B, C> Function2<A, B, C> uncurry(final Function1<A, Function1<B, C>> function1) {
        return (A a, B b) -> function1.apply(a).apply(b);
    }

    // Part 7
    static final class CurryExample {
        public static void main(final String[] args) {
            // curried
            // simple int multiplication
            final Function1<Integer, Function1<Integer, Integer>> multiplyC =
                    a -> b -> a * b;

            // use `uncurry` function to solve this
            final Function2<Integer, Integer, Integer> multiplyUC =
                    uncurry(multiplyC);

            System.out.println(multiplyUC.apply(42, 1));

            // uncurried:
            // repeat the same text specified number of times
            final Function2<Integer, String, String> replicateUC =
                    (times, str) -> {
                        final StringBuilder result = new StringBuilder();
                        for (int i = 0; i < times; i++) {
                            result.append(str);
                        }
                        return result.toString();
                    };

            final Function1<Integer, Function1<String, String>> replicateC =
                    curry(replicateUC);

            System.out.println(replicateC.apply(42).apply("JUG"));
        }
    }

    // Part 8
    static <A, B, C> Function2<B, A, C> flip(final Function2<A, B, C> function2) {
        return (B b, A a) -> function2.apply(a, b);
    }

    // Part 9
    static <A, B, C> Function1<Pair<B, A>, C> flipTupled(final Function1<Pair<A, B>, C> function1) {
        return tuple -> function1.apply(new Pair<>(tuple.second, tuple.first));
    }

    // Part 10
    static <A, B, C> Function1<B, Function1<A, C>> flipCurried(final Function1<A, Function1<B, C>> function1) {
        return (B b) -> (A a) -> function1.apply(a).apply(b);
    }
}