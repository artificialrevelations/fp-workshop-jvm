package io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_4;

import java.util.NoSuchElementException;

import static io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_4.SealedList.cons;
import static io.github.ajoz.workshop.fp.java.part_3.solutions.exercise_4.SealedList.nil;

@SuppressWarnings("WeakerAccess")
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
        public String toString() {
            return String.format("Cons(%s, %s)", head, tail.toString());
        }
    }

    private SealedList() {
    }

    public abstract A head();

    public abstract SealedList<A> tail();

    public static <A> SealedList<A> cons(final A element,
                                         final SealedList<A> list) {
        return new Cons<>(element, list);
    }

    public static <A> SealedList<A> nil() {
        return new Nil<>();
    }
}

public class Exercise4 {
    public static void main(final String[] args) {
        // A new list is created
        final SealedList<Integer> sealedList =
                cons(1, cons(2, cons(3, nil())));

        System.out.println("sealedList = " + sealedList);
    }
}