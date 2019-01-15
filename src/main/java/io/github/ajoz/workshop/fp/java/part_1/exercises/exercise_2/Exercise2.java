package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_2;

/*
  -- Advanced Functions --

  In the previous exercise we create a simple Integer to Integer function. This
  kind of type was very limiting because we had to create a function type for
  each pair of types we wanted to create an association.

  To achieve something more flexible we need generics. We can rework our
  previous example

  interface IntegerFunction {
      Integer apply(Integer argument);
  }

  We need a separate type for the argument and a separate type for the result:
 */
interface Function1<A, B> {
    B apply(A argument);
}

class Exercise2 {
    /*
      Part 1:

      Change the functions below so that they are using our new Function1 type.
     */
    static final IntegerFunction f1 = x -> x + 1;
    static final StringToIntegerFunction f2 = str -> str.length();
    static final StringFunction f3 = str -> str + "foo";

    /*
      Part 2:

      Create 3 functions:
      - str2int that takes a String and converts it to an Integer
      - int2bool that takes an Integer and converts it to a Boolean:
        * it should return true if int is equal to 42
        * it should return false otherwise
      - str2bool that takes a String and returns a Boolean:
        * this function should do what a combination of str2int and int2bool would
        * you can use str2int and int2bool to solve this exercise
     */

    

    public static void main(final String[] args) {

    }
}

interface StringToIntegerFunction {
    Integer apply(String argument);
}

interface StringFunction {
    String apply(String argument);
}

interface IntegerFunction {
    Integer apply(Integer argument);
}