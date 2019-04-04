package org.artrev.workshop.fp.part1.exercise2;


@FunctionalInterface
interface Function1<A, B> {
    B apply(A argument);
}

class Exercise2 {
    // Part 1:
    static final Function1<Integer, Integer> f1 = x -> x + 1;
    static final Function1<String, Integer> f2 = str -> str.length();
    static final Function1<String, String> f3 = str -> "foo" + str;

    // Part 2:
    static final Function1<String, Integer> str2int = Integer::valueOf;
    static final Function1<Integer, Boolean> int2bool = i -> i == 42;
    static final Function1<String, Boolean> str2bool = str -> int2bool.apply(str2int.apply(str));
    /*
    Questions:

    - Can we generify the composition of two functions somehow?
    - What types should the functions have?

    Our first function str2int is like a function from A to B
    Our second function int2bool is like a function from B to some type C
    Our third function str2bool is like a function from A to C

    We could try to generify this composition of functions with a method in the
    type Function1, or we could create some static method that would take
    two functions as arguments and return a third one.

    - Is there a problem with the function str2int?

    Yes, this function is not a total function, there is a large part of the
    function domain that it cannot return a result for. We will talk more about
    partial functions in future exercises.
     */
    // Part 3:
    static <A> Function1<A, A> identity() {
        return a -> a;
    }

    /*
    Questions:

    - Is this function useful?

    Yes!, tremendously even! It can be used anywhere we expect a function and it
    won't break anything. It can be composed with any function and not change the
    result, this is very important because we can formulate laws and assumptions
    thanks to it.

    - Can we express such function like previous ones (as a field)?

    No! Why? Java does not allow generic static fields in a class. Its obvious
    from one side but could be solved similarly to methods something like:

    static <A> Function<A, A> id = x -> x;

    vs

    static <A> Function<A, A> identity() { return x -> x; }
     */

    // Part 4:
    static <A, B> Function1<A, B> constant(final B value) {
        return a -> value;
    }

    /*
    Questions:

    - Is this function useful?

    It is not immediately obvious but this function just like identity is very
    very useful. Allows for clever composition of function in many different
    contexts.
     */
}
