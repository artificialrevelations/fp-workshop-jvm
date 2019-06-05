@file:Suppress("PackageName", "unused", "UNUSED_PARAMETER", "UNUSED_VARIABLE")

package org.artrev.workshop.fp.part4.exercise6

import org.artrev.workshop.fp.part4.exercise6.Flows.cycle
import org.artrev.workshop.fp.part4.exercise6.Flows.generate
import org.artrev.workshop.fp.tools.control.Try
import java.util.LinkedList

import java.util.Arrays.asList

/*
  -- The Game of Fizz Buzz --

  A quote from wikipedia:

  "Fizz buzz is a group word game for children to teach them about division.
  Players take turns to count incrementally, replacing any number divisible by
  three with the word "fizz", and any number divisible by five with the word
  "buzz".

  For example, a typical round of fizz buzz would start as follows:

  1, 2, Fizz, 4, Buzz, Fizz, 7, 8, Fizz, Buzz, 11, Fizz, 13, 14, Fizz Buzz, 16,
  17, Fizz, 19, Buzz, Fizz, 22, 23, Fizz, Buzz, 26, Fizz, 28, 29, Fizz Buzz, 31,
  32, Fizz, 34, Buzz, Fizz, ...
  "

  Simple enough, often used as a screening exercise on technical interviews.

  How would you write it?

  public static String fizzBuzz(fina int number) {
      if (number % 15 == 0) {
          return "fizzbuzz";
      } else if (number % 5 == 0) {
          return "buzz";
      } else if (number % 3 == 0) {
          return "fizz";
      }
      return String.valueOf(number);
  }

  Could we use the newly created Flow to solve this issue in a bit different way?

  Let's think about the game if we look closely we are working with three
  different flows of values:

  - number Flow: 1, 2, 3, 4, 5, ...
  - fizz Flow: "", "", "Fizz", ...
  - buzz Flow: "", "", "", "", "Buzz", ...

  In case of fizz and buzz Flow you can easily notice that that "cycle" into
  infinity. In case of numbers we just need to add 1 also to infinity.

  Now we could "zip" all of those Flows together to get the result we need.

  What is a Zip?
  It's an operation that creates a lazy and sequential combined Flow whose
  elements are the result of combining the elements of two flows. We need a two
  argument function to zip two Flows just like we needed a two argument function
  to fold a List.
 */
internal interface Flow<A> {
    fun next(): Try<A>
}

internal fun <A> Flow<A>.toList(): List<A> {
    val list = LinkedList<A>()
    while (true) {
        val element = next()
        if (element.isFailure)
            break

        list.add(element.get())
    }
    return list
}

/*
  Part 1:

  Please add a default method called `zip` to the Flow interface that takes
  another Flow and a two argument function and returns a Flow that is a
  representation of two flows combined. Create a class called `ZippingFlow` that
  is performing the zip operation.

  Hints:
  - You can think about zipping in terms of taking a item from first Flow, taking
    an item from the second Flow and then using the passed function to generate
    the result

  Questions:
  - Is it similar to fold operation in any way?
 */
internal class ZippingFlow<A, B, C> : Flow<C> {
    override fun next(): Try<C> =
            TODO("Exercise 6 ZippingFlow is missing!")
}

internal fun <A, B, C> Flow<A>.zip(other: Flow<B>, zipper: (A, B) -> C): Flow<C> =
        TODO("Exercise 6 Flow.zip is missing!")

/*
  Part 2:

  Please add a static method called `cycle` to the Flow interface that takes a
  List of elements and cycles through them ad infinitum. Create a class called
  `CycleListFlow` that is performing the cycle operation.

  Hints:
  - Is this similar to the ListFlow we created in the previous exercise?
  - What should happen if we reached the last element of the list?
 */
internal class CycleListFlow<A> : Flow<A> {
    override fun next(): Try<A> =
            TODO("Exercise 6 CycleListFlow is missing!")
}

/*
  Part 3:

  Please add a static method called `generate` to the Flow interface that takes
  a "seed" (initial value) and a generator function. It should allow generating
  values ad infinitum e.g.

  With a seed that is set to 1
  With a generator function that is x -> x + 1
  A Flow should be generated that can return: 1, 2, 3, 4, 5, ...

  Hints:
  - we need to keep the current value
  - we need to apply the generator function and store the result
 */
internal class GeneratingFlow<A> : Flow<A> {
    override fun next(): Try<A> =
            TODO("Exercise 6 GeneratingFlow is missing!")
}

internal object Flows {
    fun <A> cycle(list: List<A>): Flow<A> =
            TODO("Exercise 6 Flow.cycle is missing!")

    fun <A> generate(seed: A, generator: (A) -> A): Flow<A> =
            TODO("Exercise 6 Flow.generate is missing!")
}

/*
  Part 4:

  As our Flows are infinite we need to limit the amount we want to take. Please
  add a default method called `take` to the Flow interface. It should take an
  integer threshold value indicating the amount of elements that should be taken.
  Once the desired amount of elements is taken from upstream then this Flow should
  pass only Failure onwards.
 */
internal class TakeFlow<A> : Flow<A> {
    override fun next(): Try<A> =
            TODO("Exercise 6 TakeFlow is missing!")
}

internal fun <A> Flow<A>.take(threshold: Int): Flow<A> =
        TODO("Exercise 6 Flow.take is missing!")

/*
  Part 5:

  Let's build the FizzBuzz game.

  Hints:
  - how should we zip fizz's and buzz's together?
  - is the same thing for numbers and strings more tricky?
 */
fun main(args: Array<String>) {
    // First let's create the Flows of infinite data
    val fizzFlow = cycle(asList("", "", "Fizz"))
    val buzzFlow = cycle(asList("", "", "", "", "Buzz"))
    val numbers = generate(1) { a -> a + 1 }

    // Now use the implemented methods: `zip`, `take` and `toList` to
    // get a List of 200 elements
    val fizzBuzz: List<String>? = null //??

    println("fizzBuzz = " + fizzBuzz!!)
}
