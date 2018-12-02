package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_3;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Function2;

import java.util.NoSuchElementException;

import static io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_3.SealedList.cons;
import static io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_3.SealedList.nil;

abstract class SealedList<A> {
    @SuppressWarnings("WeakerAccess")
    public static class Nil<A> extends SealedList<A> {
        public Nil() {
        }

        @Override
        public A head() {
            throw new NoSuchElementException("Head of empty list!");
        }

        @Override
        public SealedList<A> tail() {
            throw new NoSuchElementException("Tail of empty list!");
        }

        @Override
        public <B> SealedList<B> map(final Function1<A, B> mapper) {
            return new Nil<>();
        }

        @Override
        public <B> B foldLeft(final B initial,
                              final Function2<B, A, B> operator) {
            return initial;
        }

        @Override
        public String toString() {
            return "Nil";
        }
    }

    @SuppressWarnings("WeakerAccess")
    public static class Cons<A> extends SealedList<A> {
        private final A head;
        private final SealedList<A> tail;

        public Cons(final A head, final SealedList<A> tail) {
            this.head = head;
            this.tail = tail;
        }

        @Override
        public A head() {
            return head;
        }

        @Override
        public SealedList<A> tail() {
            return tail;
        }

        @Override
        public <B> SealedList<B> map(final Function1<A, B> mapper) {
            // without foldLeft:
            // return cons(mapper.apply(head), tail.map(mapper));

            // with foldLeft
            return foldLeft(nil(), (list, element) -> cons(mapper.apply(element), list));
        }

        @Override
        public <B> B foldLeft(final B initial,
                              final Function2<B, A, B> operator) {
            return tail.foldLeft(operator.apply(initial, head), operator);
        }

        @Override
        public String toString() {
            return String.format("Cons(%s, %s)", head, tail.toString());
        }
    }

    private SealedList() {
    }

    public abstract A head();

    public abstract SealedList<A> tail();

    public abstract <B> SealedList<B> map(final Function1<A, B> mapper);

    public abstract <B> B foldLeft(final B initial,
                                   final Function2<B, A, B> operator);

    public static <A> SealedList<A> cons(final A element,
                                         final SealedList<A> list) {
        return new Cons<>(element, list);
    }

    public static <A> SealedList<A> nil() {
        return new Nil<>();
    }
}

public class Exercise3 {
    public static void main(final String[] args) {
        // A new list is created
        final SealedList<Integer> sealedList =
                cons(1, cons(2, cons(3, nil())));

        // List is mapped
        final SealedList<Integer> mappedList =
                sealedList.map(x -> x + 42);

        // List is folded
        final String foldedList =
                mappedList
                        .tail()
                        .foldLeft("" + mappedList.head(), (str, element) -> str + " " + element);
        System.out.println(foldedList);
    }
}
