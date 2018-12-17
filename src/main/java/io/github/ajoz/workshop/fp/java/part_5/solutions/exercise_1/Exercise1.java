package io.github.ajoz.workshop.fp.java.part_5.solutions.exercise_1;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Maybe;
import io.github.ajoz.workshop.fp.java.tools.Try;

@SuppressWarnings("unused")
final class Maybes {
    static <A, B> Function1<Maybe<A>, Maybe<B>> lift(final Function1<A, B> function) {
        return ma -> ma.map(function);
    }
}

@SuppressWarnings("unused")
final class Trys {
    static <A, B> Function1<Try<A>, Try<B>> lift(final Function1<A, B> function) {
        return ta -> ta.map(function);
    }
}

@SuppressWarnings("unused")
class Foo {
    @Deprecated
    String getData() {
        return "unsafe (can blow up) Foo data!";
    }

    Try<String> tryGetData() {
        return Try.success("safe (can still blow up but we do not care) Foo data!");
    }
}

public class Exercise1 {
    // This is our Rich and Glorious API we cannot touch!!!
    private static Function1<String, Integer> strlen = String::length;
    private static Function1<Integer, Boolean> is42 = i -> i == 42;

    // This the original implementation:
    private static Function1<Foo, Boolean> fooToBool =
            Function1.of(Foo::getData)
                    .andThen(strlen)
                    .andThen(is42);

    // This is the new implementation you want to create through composition
    private static Function1<Foo, Try<Boolean>> tryFooToBool =
            Function1.of(Foo::tryGetData)
                    .andThen(Trys.lift(strlen))
                    .andThen(Trys.lift(is42));

    public static void main(final String[] args) {
        System.out.println("fooToBool.apply(new Foo()) = " + fooToBool.apply(new Foo()));
        System.out.println("tryFooToBool.apply(new Foo()) = " + tryFooToBool.apply(new Foo()));
    }
}
