package io.github.ajoz.workshop.fp.solutions.exercise_2;

// We can mark the interface as Functional Interface, this is only an indication
// that the interface has a Single Abstract Method
@FunctionalInterface
interface Function1<A, B> {
    B apply(A argument);
}

// We can express the Function also as an abstract class:
/*
abstract class Function1<A, B> {
    abstract B apply(A argument);
}
 */

class Exercise2 {
    public static void main(String[] args) {
        //noinspection Convert2Lambda
        final Function1<String, String> function = new Function1<String, String>() {
            @Override
            public String apply(String argument) {
                return argument + "foo";
            }
        };

        // as we are using a Functional Interface then we can
        // use a very nice short notation
        final Function1<String, String> lambda =
                string -> string + "foo";

        printFunction(function, "bar");
        printFunction(lambda, "baz");
        printFunction(s -> s + "foo", "qux");

        final Function1<Integer, Integer> xPlusOne = x -> x + 1;
        final Function1<Integer, Integer> xPlusTwoSquared = x -> (x + 2) * (x + 2);
        final Function1<Integer, Integer> minusXPlusTen = x -> -x + 10;

        printFunction(xPlusOne, 1);
        printFunction(xPlusOne, 2);

        printFunction(xPlusTwoSquared, 3);
        printFunction(xPlusTwoSquared, 4);

        printFunction(minusXPlusTen, 5);
        printFunction(minusXPlusTen, 6);

        // for easier reading we will drop f and g as a name
        // and use something more descriptive
        final Function1<String, Integer> stringToInt = Integer::valueOf;
        final Function1<Integer, Boolean> intToBool = integer -> integer == 42;

        final Function1<String, Boolean> stringToBool = string -> {
            final Integer result = stringToInt.apply(string);
            return intToBool.apply(result);
        };

        printFunction(stringToBool, "41");
        printFunction(stringToBool, "42");
    }

    private static <A, B> void printFunction(final Function1<A, B> function,
                                             final A argument) {
        System.out.println(String.format("x: A = %s y: B = %s", argument, function.apply(argument)));
    }
}
