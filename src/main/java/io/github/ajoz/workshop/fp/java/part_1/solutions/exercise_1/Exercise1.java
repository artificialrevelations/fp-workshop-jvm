package io.github.ajoz.workshop.fp.java.part_1.solutions.exercise_1;

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
        final Function1 xPlusOne = new Function1() {
            @Override
            Integer apply(final Integer x) {
                return x + 1;
            }
        };

        final Function1 xPlusTwoSquared = new Function1() {
            @Override
            Integer apply(final Integer x) {
                return (x + 2) * (x + 2);
            }
        };

        final Function1 minusXPlusTen = new Function1() {
            @Override
            Integer apply(final Integer x) {
                return -x + 10;
            }
        };

        printFunction(xPlusOne, 1);
        printFunction(xPlusOne, 2);

        printFunction(xPlusTwoSquared, 3);
        printFunction(xPlusTwoSquared, 4);

        printFunction(minusXPlusTen, 5);
        printFunction(minusXPlusTen, 6);
    }

    private static void printFunction(final Function1 function,
                                      final Integer argument) {
        System.out.println(String.format("x = %d y = %d", argument, function.apply(argument)));
    }
}