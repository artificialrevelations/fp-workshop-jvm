@file:Suppress("PackageName", "unused", "UNUSED_PARAMETER")

package io.github.ajoz.workshop.fp.kotlin.part_4.exercises.practice_3

import io.github.ajoz.workshop.fp.kotlin.tools.flow.Flow
import io.github.ajoz.workshop.fp.kotlin.tools.flow.of
import io.github.ajoz.workshop.fp.kotlin.tools.flow.select

/*
  -- Looking at the flow of things --

  We already prepared many useful methods for working with the Flow class. We
  still are missing a few for:
  - peeking (or in other words looking) at the elements of the Flow without
    changing them
  - filtering of the Flow elements
  - keeping only distinct elements in the Flow
  - performing a flatMap operation on the Flow elements

  We will try to add those operations to the Flow interface that we already
  defined.
 */


/*
  Part 1:

  Implement the `flatMap` function. It should take a function that returns
  a Flow as an argument.

  Hints:
  - for each element of the upstream Flow<A> another Flow<B> is returned
  - doing simple map operation would mean returning Flow<Flow<B>>
  - Let's imagine a simple flow ["1", "2", "3"] and a function f that for
    the passed argument returns a 3 element flow containing the argument
    repeated. So for argument "1" function f would return ["1", "1", "1"]
    Doing flatMap operation on the ["1", "2", "3"] with the function f would
    mean that we would get a flow ["1", "1", "1", "2", "2", "2", "3", "3", "3"]

 */
fun <A, B> Flow<A>.flatMap(mapper: (A) -> Flow<B>): Flow<B> =
        TODO("Practice 3 Flow.flatMap is missing!")

/*
  Part 2:

  Implement the `filter` function. It should take a predicate that decides
  if the given Flow element should be passed to downstream.

  Hints:
  - if an element is not satisfying the predicate a next element should be
    checked
  - if no more elements satisfy the predicate then a Failure should be
    returned
 */
fun <A> Flow<A>.filter(predicate: (A) -> Boolean): Flow<A> =
        TODO("Practice 3 Flow.filter is missing!")

/*
  Part 3:

  Implement the `peek` function. It should take a Consumer1 as an argument,
  for every element in the flow, the passed consumer should be invoked.

  Hints:
  - does the Try have a method you need to achieve it?
 */
fun <A> Flow<A>.peek(action: (A) -> Unit): Flow<A> =
        TODO("Practice 3 Flow.peek is missing!")

/*
  Part 4:

  Implement the `distinct` function. It should return a Flow that only is
  passing distinct elements down stream.

  Hints:
  - we need to store the elements that
 */
fun <A> Flow<A>.distinct(): Flow<A> =
        TODO("Practice 3 Flow.distinct is missing!")

fun main(args: Array<String>) {
    val strings =
            Flow.of(1, 2, 3)
                    .peek { element -> println("#before filter -> $element") }
                    .select { element -> element >= 2 }
                    .peek { element -> println("#before flatMap -> $element") }
                    .flatMap { element -> Flow.of(">$element<", ">$element<") }
                    .peek { element -> println("#before distinct -> $element") }
                    .distinct()
                    .toList()
    println(strings)
}
