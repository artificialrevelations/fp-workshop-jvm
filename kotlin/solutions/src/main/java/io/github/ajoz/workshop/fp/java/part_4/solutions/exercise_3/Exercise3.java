package io.github.ajoz.workshop.fp.java.part_4.solutions.exercise_3;

import kotlin.Pair;

import java.util.concurrent.ConcurrentHashMap;

// Part 1:
interface Function1<A, B> {
    B apply(A arg);

    default Function1<A, B> memoized() {
        final ConcurrentHashMap<A, B> memo = new ConcurrentHashMap<>();
        return (A a) -> {
            if (!memo.containsKey(a)) {
                memo.put(a, this.apply(a));
            }
            return memo.get(a);
        };
    }

    static <A, B> Function1<A, B> memoize(final Function1<A, B> function) {
        return function.memoized();
    }
}

// Part 2:
interface Function2<A, B, C> {
    C apply(A arg1, B arg2);

    default Function2<A, B, C> memoized() {
        final ConcurrentHashMap<Pair<A, B>, C> memo = new ConcurrentHashMap<>();
        return (A a, B b) -> {
            final Pair<A, B> key = new Pair<>(a, b);
            if (!memo.containsKey(key)) {
                memo.put(key, this.apply(a, b));
            }
            return memo.get(key);
        };
    }

    static <A, B, C> Function2<A, B, C> memoize(final Function2<A, B, C> function) {
        return function.memoized();
    }
}

public class Exercise3 {
    public static void main(final String[] args) {
        // Part 1:
        final Function1<String, Integer> fun1 =
                Function1.memoize(arg -> {
                    System.out.println(String.format("Argument passed: %s", arg));
                    return arg.length();
                });

        for (int i = 0; i < 10; i++) {
            System.out.println(fun1.apply("JUG Lodz"));
        }

        for (int i = 0; i < 10; i++) {
            System.out.println(fun1.apply("Mobilization Conference"));
        }

        // Part 2:
        final Function2<String, String, Integer> fun2 =
                Function2.memoize((arg1, arg2) -> {
                    System.out.println(String.format("Arguments passed: %s, %s", arg1, arg2));
                    return (arg1 + arg2).length();
                });

        for (int i = 0; i < 10; i++) {
            System.out.println(fun2.apply("JUG", "Lodz"));
        }

        for (int i = 0; i < 10; i++) {
            System.out.println(fun2.apply("Mobilization", "Conference"));
        }
    }
}
