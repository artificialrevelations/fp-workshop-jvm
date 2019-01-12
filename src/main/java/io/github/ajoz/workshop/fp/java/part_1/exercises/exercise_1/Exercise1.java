package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_1;

/*
  -- Simple Functions --

  In Java we have a few ways to express a function:
  - as an abstract class
  - as an interface

  Let's think about one argument functions first. They take a single argument
  and return a single result ;-)

  A type that can represent a function from Integer to Integer can look like:

  abstract class IntegerFunction {
      public abstract Integer apply(Integer argument);
  }

  This is a super verbose definition let's use an interface instead:
  */
interface IntegerFunction {
    Integer apply(Integer argument);
}

/*
  We can now use our newly created interface to create few functions:
  - y = x + 1
  - y = (x + 2)^2
  - y = -x + 10
  - y = x^2 + 4x + 1
  */

class Exercise1 {
    // y = x + 1
    @SuppressWarnings("Convert2Lambda")
    static final IntegerFunction f1 = new IntegerFunction() {
        @Override
        public Integer apply(final Integer argument) {
            return argument + 1;
        }
    };

    /*
    Java 8 introduced few nice things, one of which are lambda expressions. They
    allow for a more concise way of writing functions. A lambda expression in
    Java consists of the argument or arguments (more on that later) a special
    "arrow" symbol "->" and a body of a lambda. Let's see how the function `f1`
    would look in the lambda form:

    private static final IntegerFunction f1 = x -> x + 1

    There is no return statement if the body of the lambda has only "single line"
    of code. If the lambda would need more lines then a pair of bracers would be
    needed:

    private static final IntegerFunction f1 = x -> {
        // some special code
        // some even more special code
        return x + 1;
    };

    In this situation a `return` statement is needed to mark what should be
    returned as a result of the function.
    */

    // y = (x + 2)^2
    static final IntegerFunction f2 = x -> {
        throw new UnsupportedOperationException("Exercise 1 function f2 body is missing!");
    };

    // y = -x + 10
    static final IntegerFunction f3 = x -> {
        throw new UnsupportedOperationException("Exercise 1 function f3 body is missing!");
    };

    // y = x^2 + 4x + 1
    static final IntegerFunction f4 = x -> {
        throw new UnsupportedOperationException("Exercise 1 function f4 body is missing!");
    };

    /*
    What about different types of functions? From Double to Double, from Float
    to Float or from String to String? What about more exciting functions from
    String to Integer or from MyType to YourType?
     */
}

