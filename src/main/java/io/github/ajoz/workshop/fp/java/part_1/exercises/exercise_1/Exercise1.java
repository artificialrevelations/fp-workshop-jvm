package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_1;

/*
  -- Simple Functions --

  Create a class that can represents a one argument function from Integer to Integer.

  Express the function y = x + 1 with your class.
  Express the function y = (x + 2)^2 with your class.
  Express the function y = -x + 10 with your class.

  Expressing only simple Integer to Integer functions does not look very powerful
  and useful can this Function definition be enhanced to work on any type?
 */

interface Function {
    Integer invoke(Integer input);
}

public class Exercise1 {
    public static void main(String[] args) {

        // region task1
        Function f = new Function() {
            @Override
            public Integer invoke(Integer input) {
                return input + 1;
            }
        };

        Function lambdaF = input -> input + 1;
        printFunction(2, lambdaF);
        // endregion

        // region task2
        Function lambdaG = input -> (input + 2) * (input +2);
        printFunction(3, lambdaG);
        // endregion

        // region task3
        Function h = input -> -input + 10;
        printFunction(4, h);
        // endregion
    }

    private static void printFunction(final Integer argument,
                                      final Function function) {
        System.out.println(String.format("f(%d) = %d ", argument, function.invoke(argument)));
    }
}