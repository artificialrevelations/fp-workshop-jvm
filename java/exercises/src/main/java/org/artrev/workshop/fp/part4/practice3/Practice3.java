package org.artrev.workshop.fp.part4.practice3;

import org.artrev.workshop.fp.tools.Consumer1;
import org.artrev.workshop.fp.tools.Function1;
import org.artrev.workshop.fp.tools.Predicate;
import org.artrev.workshop.fp.tools.control.Try;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

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
@SuppressWarnings("unused")
interface Flow<A> {
    Try<A> next();

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
    default <B> Flow<B> flatMap(final Function1<A, Flow<B>> mapper) {
        throw new UnsupportedOperationException("Practice 3 Flow.flatMap is missing!");
    }

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
    default Flow<A> filter(final Predicate<A> predicate) {
        throw new UnsupportedOperationException("Practice 3 Flow.filter is missing!");
    }

    /*
      Part 3:

      Implement the `peek` function. It should take a Consumer1 as an argument,
      for every element in the flow, the passed consumer should be invoked.

      Hints:
      - does the Try have a method you need to achieve it?
     */
    default Flow<A> peek(final Consumer1<A> action) {
        throw new UnsupportedOperationException("Practice 3 Flow.peek is missing!");
    }

    /*
      Part 4:

      Implement the `distinct` function. It should return a Flow that only is
      passing distinct elements down stream.

      Hints:
      - we need to store the elements that
     */
    default Flow<A> distinct() {
        throw new UnsupportedOperationException("Practice 3 Flow.distinct is missing!");
    }

    // Not Part of the exercise:
    static <A> Flow<A> of(final List<A> list) {
        return new ListFlow<>(list);
    }

    default List<A> toList() {
        final List<A> list = new LinkedList<>();
        while (true) {
            final Try<A> element = next();
            if (element.isFailure())
                break;

            list.add(element.get());
        }
        return list;
    }
}

final class ListFlow<A> implements Flow<A> {
    private final List<A> list;
    private int current;

    ListFlow(final List<A> list) {
        this.list = list;
    }

    @Override
    public Try<A> next() {
        if (list.size() == 0)
            return Try.failure(new NoSuchElementException("No elements in this Flow!"));

        if (current >= list.size())
            return Try.failure(new NoSuchElementException("No more elements in this Flow!"));

        final Try<A> next = Try.success(list.get(current));
        current++;
        return next;
    }
}

public class Practice3 {
    public static void main(String[] args) {
        final List<String> strings =
                Flow.of(Arrays.asList(1, 2, 3))
                        .peek(element -> System.out.println("#before filter -> " + element))
                        .filter(element -> element >= 2)
                        .peek(element -> System.out.println("#before flatMap -> " + element))
                        .flatMap(element -> Flow.of(Arrays.asList(">" + element + "<", ">" + element + "<")))
                        .peek(element -> System.out.println("#before distinct -> " + element))
                        .distinct()
                        .toList();
        System.out.println("strings = " + strings);
    }
}
