package io.github.ajoz.workshop.fp.part1.exercise2

/*
  -- Advanced Functions --

  In the previous example we played a bit with some simple (Int) -> Int functions.
  We even created a strlen function that was a (String) -> Int.

  A type that can represent a function from some generic type A to some generic
  type B can look like:

  interface KFunction1<in A, out B> {
    fun invoke(argument: A): B
  }

  We do not have to create such type on our own, Kotlin covers us here and its
  possible to specify a function (A) -> B that is referring to generic types.

  fun <A, B> foo(f: (A) -> B) {
    // some code
  }

  This will be very useful when we will start to enhance the type with some
  useful extension functions.

  Part 1:

  Please create 3 functions:
  - f1 that takes an Int and returns an Int, this function should increment its
    argument by one.
  - f2 that takes a String and returns an Int, this function should return the
    length of the given argument.
  - f3 that takes a String and returns a String, this function should prepend
    text "foo" to the given argument.

  Uncomment the tests!
 */

// val f1: (???) -> ??? = ???
// val f2: (???) -> ??? = ???
// val f3: (???) -> ??? = ???

/*
  Part 2:

  Create 3 functions:
  - str2int that takes a String and converts it to an Int
  - int2bool that takes an Int and converts it to a Boolean:
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

// val str2int: (???) -> ??? = ???
// val int2bool: (???) -> ??? = ???
// val str2bool: (???) -> ??? = ???