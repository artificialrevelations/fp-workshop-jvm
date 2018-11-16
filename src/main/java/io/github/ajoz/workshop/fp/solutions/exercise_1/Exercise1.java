package io.github.ajoz.workshop.fp.solutions.exercise_1;

// We can express a one argument function in the form of an abstract class:
abstract class Function1 {
    abstract Integer apply(Integer argument);
}


// We can express a one argument function in the form of an interface:
/*
interface Function1 {
    Integer apply(Integer argument);
}
*/

public class Exercise1 {
    public static void main(String[] args) {
        final Function1 function = new Function1() {
            @Override
            Integer apply(final Integer x) {
                return x + 1;
            }
        };

        printFunction(function, 1);
        printFunction(function, 2);
        printFunction(function, 3);
        printFunction(function, 4);
    }

    private static void printFunction(final Function1 function,
                                      final Integer argument) {
        System.out.println(String.format("x = %d y = %d", argument, function.apply(argument)));
    }
}