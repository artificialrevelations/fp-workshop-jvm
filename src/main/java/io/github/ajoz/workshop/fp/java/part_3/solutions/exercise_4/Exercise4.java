package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_4;

/*
  -- Partial and Total Functions

  A partial function from X to Y is a function f: X' -> Y for some proper subset
  X' of X. If X' = X, then f is called a total function.

  This definition sounds very "math-y" but in plain words it means that partial
  functions are those that do not have "results" for every argument in their
  domain.

  Do such functions exist?

  The simplest example is division. We cannot divide by zero. So division is a
  partial function on the domain of natural numbers but is a total function on
  the domain of natural numbers without zero.

  But this is "math-y", what about real world engineering examples?

  Are these functions partial?
  - getting Order information from the Database
  - getting Order information from the Cache
  - getting Order information from the Server
  - getting information from a Device
  - getting value from a Map under the given key
  - getting first value from a List (head)
  - getting the List except the head (tail)
  - getting List item on the given index
  - getting element from a Set
  - getting value stored in SharedPreferences (Android)

  This looks like a lot of things we are usually working with.
 */
public class Exercise4 {
    /*
      Part 1:

      Please create a function called `div1` that performs division of two
      given Integers.

      Question:
      - what will happen if we divide by zero?
      - is having an implicit exception in the code a good thing?
      - how can we solve this issue?
     */
    static Integer div1(final Integer a, final Integer b) {
        return a / b;
    }

    /*
      Part 2:

      Please create a function called `div2` that performs division of two
      given Integers but in the case of the second argument being zero throws
      a checked exception called DivideByZero.

      Question:
      - is using a checked exception better?
      - what about the code readability?
      - what about the code extendability?
      - is it easy to use the `div2` function with other functions?
     */
    static class DivideByZero extends Exception {
    }

    static Integer div2(final Integer a, final Integer b) throws DivideByZero {
        if (b == 0)
            throw new DivideByZero();

        return a / b;
    }

    /*
      Part 3:

      Please create a function called `div3` that performs division of two
      given Integers but in the case of the second argument being zero it
      returns a null.

      Question:
      - is it better then the checked exception version?
      - what about the code readability?
      - what about the code extendability?
      - is it easier to use then `div2`?
     */
    static Integer div3(final Integer a, Integer b) {
        return b != 0 ? a / b : null;
    }

    /*

     */
    static class Result {
        public final Integer value;
        public final boolean exists;

        public Result(Integer value, boolean exists) {
            this.value = value;
            this.exists = exists;
        }
    }

    static Result div4(final Integer a, final Integer b) {
        return b != 0
                ? new Result(a / b, true)
                : new Result(null, false);
    }

    public static void main(String[] args) {
        // Part 1:
        System.out.println(div1(42, 0));

        // Part 2:
        try {
            final Integer res2 = div2(42, 0);
            System.out.println(res2);
        } catch (DivideByZero divideByZero) {
            divideByZero.printStackTrace();
        }

        // Part 3:
        final Integer res3 = div3(42, 0);
        if (null != res3) {
            System.out.println("Div3 result: " + res3);
        } else {
            System.out.println("Error handling after div3 failed!");
        }

        // Part 4:
        final Result res4 = div4(42, 0);
        if (res4.exists) {
            System.out.println("Div4 result: " + res4);
        } else {
            System.out.println("Error handling after div4 failed!");
        }
    }
}
