package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_2;

/*
  -- Advanced Functions --

  Create a class that can represents a one argument function from a generic type
  A to a generic type B

  - part 1:
  Express the function from String to String that is adding word "foo" to every
  argument. Use it to concatenate: "bar", "baz", "qux"

  - part 2:
  Use the new Function type to express the functions from the first exercise:
  1) y = x + 1
  2) y = (x + 2)^2
  3) y = -x + 10

  - part 3:
  Create functions:
  1) f that takes a String and converts it to an Integer
  2) g that takes an Integer and converts it to a Boolean - true if it is 42, false
     otherwise
  3) h that takes a String and returns a Boolean - h should do what a combination
     of f and g would, you can use f and g to solve this.

  Is there a problem with function f?
  Can we combine f and g in a meaningful way?
 */

interface Function<S, R> {
    R invoke(S input);
}

public class Exercise2 {
    public static void main(String[] args) {

        // region part1
        Function<String, String> str2str = input -> input + "foo";
        printFunction("bar", str2str);
        printFunction("baz", str2str);
        printFunction("qux", str2str);
        // endregion

        // region part 2
        Function f = new Function<Integer, Integer>() {
            @Override
            public Integer invoke(Integer input) {
                return input + 1;
            }
        };

        Function<Integer, Integer> lambdaF = input -> input + 1;
        printFunction(2, lambdaF);

        Function<Integer, Integer> lambdaG = input -> (input + 2) * (input +2);
        printFunction(3, lambdaG);

        Function<Integer, Integer> h = input -> -input + 10;
        printFunction(4, h);
        // endregion

        // region part 3
        Function<String, Integer> f1 = Integer::valueOf;
        printFunction("31", f1);

        Function<Integer, Boolean> g = input -> input == 42;
        printFunction(42, g);
        printFunction(43, g);

        Function<String, Boolean> h1 = (String input) -> g.invoke(f1.invoke(input));
        printFunction("43", h1);
        printFunction("42", h1);
        // endregion
    }

    private static <S, R> void printFunction(final S argument,
                                             final Function<S, R> function) {
        System.out.println(String.format("f(%s) = %s ", argument, function.invoke(argument)));
    }
}