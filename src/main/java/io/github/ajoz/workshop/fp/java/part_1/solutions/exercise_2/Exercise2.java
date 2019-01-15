package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_2;


@FunctionalInterface
interface Function1<A, B> {
    B apply(A argument);
}

class Exercise2 {
    // Part 1:
    static final Function1<Integer, Integer> f1 = x -> x + 1;
    static final Function1<String, Integer> f2 = str -> str.length();
    static final Function1<String, String> f3 = str -> str + "foo";

    // Part 2:
    static final Function1<String, Integer> str2int = Integer::valueOf;
    static final Function1<Integer, Boolean> int2bool = i -> i == 42;
    static final Function1<String, Boolean> str2bool = str -> int2bool.apply(str2int.apply(str));
    /*
    Questions:

    Can we generify the composition of two functions somehow?
    What types should the functions have?

    Our first function str2int is like a function from A to B
    Our second function int2bool is like a function from B to some type C
    Our third function str2bool is like a function from A to C

    We could try to generify this composition of functions with a method.
     */
}
