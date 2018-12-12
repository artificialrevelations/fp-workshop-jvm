package io.github.ajoz.workshop.fp.java.part_4.solutions.exercise_6;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Function2;
import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

import static io.github.ajoz.workshop.fp.java.part_4.solutions.exercise_6.Flow.cycle;
import static io.github.ajoz.workshop.fp.java.part_4.solutions.exercise_6.Flow.generate;
import static java.util.Arrays.asList;

interface Flow<A> {
    Try<A> next();

    default <B, C> Flow<C> zip(final Flow<B> other,
                               final Function2<A, B, C> zipper) {
        return new ZippingFlow<>(this, other, zipper);
    }

    default Flow<A> take(final int threshold) {
        return new TakeFlow<>(this, threshold);
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

    static <A> Flow<A> cycle(final List<A> list) {
        return new CycleListFlow<>(list);
    }

    static <A> Flow<A> generate(final A seed,
                                final Function1<A, A> generator) {
        return new Seed1Flow<>(seed, generator);
    }
}

class ZippingFlow<A, B, C> implements Flow<C> {
    private final Flow<A> left;
    private final Flow<B> right;
    private final Function2<A, B, C> zipper;

    ZippingFlow(final Flow<A> left,
                final Flow<B> right,
                final Function2<A, B, C> zipper) {
        this.left = left;
        this.right = right;
        this.zipper = zipper;
    }

    @Override
    public Try<C> next() {
        final Try<A> nextLeft = left.next();
        if (nextLeft.isFailure())
            return Try.failure(new NoSuchElementException("Left seq is out of elements to zip!"));

        final Try<B> nextRight = right.next();
        if (nextRight.isFailure())
            return Try.failure(new NoSuchElementException("Right seq is out of elements to zip!"));

        return nextLeft.flatMap(a -> nextRight.flatMap(b -> Try.success(zipper.apply(a, b))));
    }
}

class CycleListFlow<A> implements Flow<A> {
    private final List<A> list;
    private int current;

    CycleListFlow(final List<A> list) {
        this.list = list;
    }

    @Override
    public Try<A> next() {
        if (list.size() == 0)
            return Try.failure(new NoSuchElementException("No elements in this Flow!"));

        if (current >= list.size())
            current = 0;

        final Try<A> next = Try.success(list.get(current));
        current++;
        return next;
    }
}

class Seed1Flow<A> implements Flow<A> {
    private final Function1<A, A> generator;
    private A seed;

    public Seed1Flow(final A seed,
                     final Function1<A, A> generator) {
        this.seed = seed;
        this.generator = generator;
    }

    @Override
    public Try<A> next() {
        final Try<A> next = Try.success(seed);
        seed = generator.apply(seed);
        return next;
    }
}

class TakeFlow<A> implements Flow<A> {
    private final Flow<A> upstream;
    private final int amount;

    private int taken;

    TakeFlow(final Flow<A> upstream,
             final int amount) {
        this.upstream = upstream;
        this.amount = amount;
    }

    @Override
    public Try<A> next() {
        // take an element from the upstream Flow
        final Try<A> next = upstream.next();
        // if the elemenent does not exist then just propagate the failure
        if (next.isFailure())
            return next;

        // if already taken enough elements then just propagate the failure
        if (taken >= amount) {
            return Try.failure(new NoSuchElementException("Reached the Flow amount: " + amount));
        }

        // increment the currently taken amount
        taken++;
        // return the upstream Flow item
        return next;
    }
}

public class Exercise6 {
    public static void main(final String[] args) {
        final Flow<String> fizzFlow = cycle(asList("", "", "Fizz"));
        final Flow<String> buzzFlow = cycle(asList("", "", "", "", "Buzz"));
        final Flow<Integer> numbers = generate(1, a -> a + 1);

        final List<String> fizzBuzz =
                fizzFlow
                        .zip(buzzFlow, (a, b) -> a + b)
                        .zip(numbers, (s, integer) -> {
                            if (s.isEmpty())
                                return String.valueOf(integer);

                            return s;
                        })
                        .take(200)
                        .toList();

        System.out.println("fizzBuzz = " + fizzBuzz);
    }
}
