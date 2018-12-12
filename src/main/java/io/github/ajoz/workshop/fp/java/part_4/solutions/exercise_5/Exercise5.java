package io.github.ajoz.workshop.fp.java.part_4.solutions.exercise_5;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

interface Flow<A> {
    Try<A> next();

    default <B> Flow<B> map(final Function1<A, B> mapper) {
        return new MapFlow<>(this, mapper);
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

    static <A> Flow<A> of(final List<A> list) {
        return new ListFlow<>(list);
    }
}

class ListFlow<A> implements Flow<A> {
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

class MapFlow<A, B> implements Flow<B> {
    private final Flow<A> upstream;
    private final Function1<A, B> mapper;

    MapFlow(final Flow<A> upstream,
            final Function1<A, B> mapper) {
        this.upstream = upstream;
        this.mapper = mapper;
    }

    @Override
    public Try<B> next() {
        return upstream.next().map(mapper);
    }
}

public class Exercise5 {
    public static void main(final String[] args) {
        final List<Integer> l1 =
                Flow.of(Arrays.asList(1, 2, 3)).toList();
        System.out.println("Flow.of([1, 2, 3]) = " + l1);

        final List<Integer> l2 =
                Flow.of(Arrays.asList("JUG", "Lodz")).map(String::length).toList();

        System.out.println("Flow.of([\"JUG\", \"Lodz\"]).map(String::length) = " + l2);
    }
}
