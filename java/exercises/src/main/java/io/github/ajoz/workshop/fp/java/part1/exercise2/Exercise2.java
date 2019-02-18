package io.github.ajoz.workshop.fp.java.part1.exercise2;

/*
  -- Advanced Functions --

  In the previous exercise we create a simple Integer to Integer function. This
  kind of type was very limiting because we had to create a function type for
  each pair of types we wanted to create an association:
  */
@FunctionalInterface
interface StringToIntegerFunction {
    Integer apply(String argument);
}

@FunctionalInterface
interface StringFunction {
    String apply(String argument);
}

@FunctionalInterface
interface IntegerFunction {
    Integer apply(Integer argument);
}

/*
  To achieve something more flexible we need generics. We can rework our
  "function" types into something that will use generic type for the argument
  and a generic type for the result.
  */
@FunctionalInterface
interface Function1<A, B> {
    B apply(A argument);
}

@SuppressWarnings("Convert2MethodRef")
class Exercise2 {
    /*
      Part 1:

      Change the functions below so that they are using our new Function1 type.
     */
    static final IntegerFunction f1 = x -> x + 1;
    static final StringToIntegerFunction f2 = str -> str.length();
    static final StringFunction f3 = str -> "foo" + str;

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

      Uncomment tests for this part!

      Hints:
      - to convert String to an Integer please use Integer.valueOf(String)

      Questions:
      - Can we generify the composition of two functions somehow?
      - What types should the functions have?
      - Is there a problem with the function str2int?
     */

    // static final Function<???, ???> str2int = ???
    // static final Function<???, ???> int2bool = ???
    // static final Function<???, ???> str2bool = ???

    /*
      Part 3:

      Create a method called identity that returns a function. This function
      should take an argument and return it immediately. This function should
      work for any type passed to it.

      Questions:
      - Is this function useful?
      - Can we express such function like previous ones (as a field)?
     */
    // static Function<???, ???> identity() {}

    /*
      Part 4:

      Create a method called constant that takes a value and returns a function
      that always returns this value regardless of the arguments that are passed
      to it.

      Questions:
      - Is this function useful?
     */
    // static Function<???, ???> constant(??? value) {}
}