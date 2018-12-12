package io.github.ajoz.workshop.fp.java.part_4.exercises.exercise_5;

import io.github.ajoz.workshop.fp.java.tools.Try;

/*
  -- Another form of laziness --

  We managed to create a lazy `Supplier<A>` and even add some useful operations
  to it. Working with it's return value doesn't have to mean that we need to
  run it.

  It would be nice to achieve this with a List? or an Array?

  Java 8 introduced a concept of a Stream. RxJava introduced the concept of an
  Observable. Both can allow a certain form of laziness but they differ greatly.

  RxJava represents the "reactive" approach, one could say its more of a PUSH
  based processing. Values are pushed from upstream to downstream.

  Java 8 Stream is an example of PULL based processing. Values are pulled from
  upstream by downstream. We can set Stream up with all the operations we want
  to perform, but they won't happen until we explicitly ask for them.

  Stream API recognizes a concept of terminal operation.

  It is an operation that forces the Stream values to be computed. Let's think
  what kind of operations that we implemented for other types can be terminal.

  Questions:
  - Is `map` a terminal operation?
  - Is `flatMap` a terminal operation?
  - Is `filter` a terminal operation?
  - Is `foldLeft` a terminal operation?
  - Is `reduce` a terminal operation?

  We will focus on implementing a simple Stream ourselves. We will call it Flow
  to distinguish it from Java's Stream and Kotlin's Sequence.

  Questions:
  - How should it be designed?
  - Compare Flow with the Supplier we implemented
  - Compare Flow with the Maybe and Try we implemented
  - What would we want?
  - How would the API look like?

  Let's first write what do we want from it:
  - operations need to be lazy
  - operations need to be only performed if necessary
  - it should be possible to create infinite Flows
  - it should be possible to change a collection into a Flow
  - it should be possible to change an array into a Flow
  - operations need to be "chainable" like in Stream, Sequence or Observable
  - it should be possible to work with any type in the Flow

  So how it would look like:

  final Flow<String> strings = new Flow("JUG", "Lodz");
  final Flow<Integer> lengths = strings.map(String::length);

  final List<Integer> lenList = lengths.toList();

  or

  final List<Integer> lenList =
                    Flow.of("JUG", "Lodz")
                        .map(String::length)
                        .toList();

  Because fluent APIs are in fashion nowadays.

  How to achieve it? We need a chain of operations:

  Data -> Operation 1 -> Operation 2 -> ... -> Operation N -> Perform

  In the previous lessons we explored the idea of recursive data types. The gist
  of it was that the type had reference to another thing of the same type. We
  could go a similar route with our Flow type.

  Flow:
  - either is wrapping some Data
  - either is wrapping another Flow and an operation it needs to perform ont

  This would mean that that the Flow would look like:

  Flow(Flow(Flow(Flow(Data), Operation 1), Operation 2), ...)

  Ok this is fine but how to make it lazy?

  Maybe we should try to take one one element at a time? There is already an
  interface in Java that is performing operations in a similar way.

  interface Iterator<A> {
      boolean hasNext();
      A next();
  }

  It is a bit troublesome to work with because the logic needs to be split into
  two methods. It would be nice if we could wrap uncertainty of getting a result
  into one simple type. I think we have such thing available.

  Let's rework the Iterator interface a bit:
 */

interface Flow<A> {
    Try<A> next();
}

/*
  We managed to bake hasNext checks into a single call. Let's start adding
  missing methods.

  Part 1:

  Please add terminal default method called `toList()` to the Flow interface.
  It should return a List of type A from the given Flow.

  Hints:
  Let's think how did we use Iterator<A>?

  final Iterator<A> iter = ...

  while(iter.hasNext()) {
      final A element = iter.next();
      // do something with the element
  }

  In the Flow interface we have everything baked into one method and a single
  type. Try is either Success or Failure.

  This can be done similarly with Flow, we need to ask for the next element of
  the Flow until its a Failure.
 */

/*
  Part 2:

  We can change a Flow into a List, but we don't have a way to create a Flow.

  Please add a static method called `of` to Flow interface. It should take a
  List<A> as an argument and return a Flow<A>. Flow should be able to traverse
  the list one element at a time.

  If the List is empty or there are no more elements to send a Failure should
  be passed.

  Instead of implementing this in place, please create a class called ListFlow<A>
  that implements Flow<A> interface.

  This static method `of` should return an instance of a `ListFlow<A>` as a result.

  Hints:
  - We need to take the List as an argument
  - We need to somehow store the information what is the next element of the list
    to send downstream (maybe mutable?)
  - We need to send it one by one
 */
//class ListFlow implements Flow {
//
//}

/*
  Part 3:

  We can create Flows, we can change them back to standard Java types. Now let's
  add something useful to them. We will start with `map`. We want to take a
  Flow<A> and Function1<A, B> to transform it to Flow<B>.

  Remember we need to do it one element at a time.

  Please add a default method called `map` to the Flow interface.
  Please create a class `MapFlow` that implements Flow interface and performs
  the mapping.

  Hints:
  - what should be the type of the resulting Flow
  - Should the MapFlow use generics?
  - We need to process one element at a time, can we use Try to make it easier?
 */
//class MapFlow implements Flow {
//
//}

public class Exercise5 {
    public static void main(final String[] args) {
//        final List<Integer> l1 =
//                Flow.of(Arrays.asList(1, 2, 3)).toList();
//        System.out.println("Flow.of([1, 2, 3]) = " + l1);
//
//        final List<Integer> l2 =
//                Flow.of(Arrays.asList("JUG", "Lodz")).map(String::length).toList();
//
//        System.out.println("Flow.of([\"JUG\", \"Lodz\"]).map(String::length) = " + l2);
    }
}
