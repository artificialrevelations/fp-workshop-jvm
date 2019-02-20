package io.github.ajoz.workshop.fp.part1.exercise1;

// Part 1 (interface)
interface IntegerFunction {
    Integer apply(Integer argument);
}

// Part 2 (interface)
interface StringToIntegerFunction {
    Integer apply(String argument);
}

class Exercise1 {
    // Part 1:
    // y = x + 1
    static final IntegerFunction f1 = x -> x + 1;

    // y = (x + 2)^2
    static final IntegerFunction f2 = x -> (x + 2) * (x + 2);

    // y = -x + 10
    static final IntegerFunction f3 = x -> -x + 10;

    // y = x^2 + 4x + 1
    static final IntegerFunction f4 = x -> x * x + 4 * x + 1;

    // Part 2:
    static final StringToIntegerFunction strlen = str -> str.length();
    // this can be also expressed with a method reference
    // static final StringToIntegerFunction strlen = String::length;
}