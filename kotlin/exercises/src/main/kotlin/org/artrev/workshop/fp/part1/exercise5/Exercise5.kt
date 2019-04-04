@file:Suppress("unused", "UNUSED_PARAMETER", "UNUSED_VARIABLE", "UnusedMainParameter")

package org.artrev.workshop.fp.part1.exercise5

/*
  -- Partial Application --

  From one point of view two argument functions are problematic because we always
  need to supply it with both arguments. Like you can imagine the problem escalates
  quickly with functions of larger arity.

  We already explored few ways of changing functions with arities greater then
  one to a one argument function.

  For the task at hand the most useful would be the curried form of a two or more
  argument function. Function of the form:

  (A) -> (B) -> C

  Can be partially applied. It means that the first argument can be supplied
  without passing the other one. As a result of passing the argument A we would
  get the result of (B) -> C

  Example:

  For example let's say you have a certain class that is responsible for loading
  some data let's call it a Loader, this class accepts instances of Foo and returns
  instances of Bar.

  interface Loader {
      // some class
      fun loadInfo(foo: Foo): Bar {
          // some heavy business logic
      }
  }

  Now somewhere in the code you are processing a List<Foo> and you need a List<Bar>.
  That code expects a (Foo) -> Bar:

  public void doSomeMightyProcessing(function: (Foo) -> Bar) {

  We would like to use our Loader class and pass a correct (Foo) -> Bar
  to the `doSomeMightyProcessing`.

  Let's create a two argument curried Function:

  val loadBarFromFoo: (Loader) -> (Foo) -> Bar =
        { loader -> { foo -> loader.loadInfo(foo) } }

  Now we can partially apply the function with a correct Loader instance

  barToFoo: (Foo) -> Bar = loadBarFromFoo(someParticularLoader);

  doSomeMightyProcessing(barToFoo);
 */

/*
  Part 1:

  Please create an extension function called `applyFirst` on a curried two
  argument function that takes the first argument. This function should return
  a one argument function as the result.
 */
fun <A, B, C> ((A) -> (B) -> C).applyFirst(value: A): (B) -> C =
        TODO("Exercise 5 applyFirst is missing!")

/*
  Part 2:

  Please create an extension function called `applySecond` on a a curried two
  argument function that takes the second argument. This method should return
  a one argument function as the result.
 */
fun <A, B, C> ((A) -> (B) -> C).applySecond(value: B): (A) -> C =
        TODO("Exercise 5 applySecond is missing!")

/*
  Part 3:

  Applying the first argument in a curried function is trivial. A bit less
  trivial in case of the second.
 */
object Part3 {
    @JvmStatic
    fun main(args: Array<String>) {
        // Partially apply `addInts` function so you will get as a result
        // a function that always adds 1 to whatever it gets as an argument.
        val addInts: (Int) -> (Int) -> Int = { a -> { b -> a + b } }

        val addOne: (Int) -> Int =
                { TODO("Exercise 5 addOne is missing!") }

        println(addOne(0))
        println(addOne(1))
        println(addOne(41))

        // Partially apply `concatStrings` so you will get as a result a function
        val concatStrings: (String) -> (String) -> String =
                { first -> { second -> first + second } }

        // that adds a "foo" prefix to any String argument
        val fooPrefix: (String) -> String =
                { TODO("Exercise 5 fooPrefix is missing!") }

        // that adds a "bar" suffix to any String argument
        val barSuffix: (String) -> String =
                { TODO("Exercise 5 fooPrefix is missing!") }

        println(fooPrefix("rever with JUG Łódź!"))
        println(barSuffix("Unfortunately no sponsors for an open "))
    }
}

/*
  Part 4:

  Although we mentioned about partial application in the context of the
  one argument function. It can be achieved for a two argument function.

  Please add an extension function called `applyFirst` to the two argument
  function. This extension function should return a one argument function as a
  result.
 */
fun <A, B, C> ((A, B) -> C).applyFirst(value: A): (B) -> C =
        TODO("Exercise 5 applyFirst is missing!")

/*
  Part 5:

  Please add an extension function called `applySecond` to the two argument
  function. This extension function should return a one argument function as a
  result.

  Questions:
  - Can a two argument function be composed in any meaningful way?
 */
fun <A, B, C> ((A, B) -> C).applySecond(value: B): (A) -> C =
        TODO("Exercise 5 applySecond is missing!")

object Part6 {
    /*
      Part 6:

      Please use the newly created extension functions `applyFirst` or
      `applySecond` to compose a two argument function with a one argument
      function.
     */
    @JvmStatic
    fun main(args: Array<String>) {
        // this function drops a certain amount of chars from a given string
        val drop: (String, Int) -> String = { string, amount ->
            val length = string.length
            if (amount < length)
                string.substring(amount, length)
            else
                ""
        }

        val length: (String) -> Int = { it.length }

        // We want to compose function `drop` with function `length`
        // We always want to drop 6 first characters from a given string

        val substrlen: (String) -> Int =
                { TODO("Exercise 5 substrlen is missing!") }

        println(substrlen("This exercise is very easy?"))
    }
}
