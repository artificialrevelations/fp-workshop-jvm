package io.github.ajoz.workshop.fp.java.part_3.exercises.exercise_4;

import static io.github.ajoz.workshop.fp.java.part_3.exercises.exercise_4.SealedList.cons;
import static io.github.ajoz.workshop.fp.java.part_3.exercises.exercise_4.SealedList.nil;

/*
  -- Recursive types --

  We will create a single linked list in the form of an ADT. How a single linked
  list is built? It's built from nodes, each node holds a value and a reference
  to the next node. This means that the last node will have a reference to the
  next set to null. But what if we would like to not use null for anything?

  A quick dip into Haskell:

  data List a = Nil | Cons a (List a)

  List is either empty (Nil) or has an element and a reference to the next one
  (Cons).

  Empty list is just Nil.
  Single element list is Cons(element Nil)
  Two element list is Cons(element1 Cons(element2 Nil))
  etc.

  Simple List 1, 2, 3 will be

  Cons(1, Cons(2, Cons(3, Nil)))

  List is a very special example as it is simple yet shows both sum and product
  in one concise package:

  List = 1 + a * List

  It is also an example of a very interesting type - a recursive type.

  Let's create one in Java.
 */
@SuppressWarnings({"WeakerAccess", "unused"})
abstract class SealedList<A> {
    public static class Nil<A> extends SealedList<A> {
        private Nil() {
        }

        /*
          Part 1:

          Add missing `head` and `tail` methods for Nil case.

          Head of the list is the first element of the list.
          In case of an empty List (Nil) a NoSuchElementException should be thrown.

          Tail of the list is all the elements except the head.
          In case of an empty List (Nil) a NoSuchElementException should be thrown.
         */
        @Override
        public A head() {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Nil.head is missing!");
        }

        @Override
        public SealedList<A> tail() {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Nil.tail is missing!");
        }

        @Override
        public String toString() {
            return "Nil";
        }
    }

    public static class Cons<A> extends SealedList<A> {
        private final A head;
        private final SealedList<A> tail;

        public Cons(final A head, final SealedList<A> tail) {
            this.head = head;
            this.tail = tail;
        }

        /*
          Part 2:

          Add missing `head` and `tail` methods for Cons case.

          Questions:
          - should we be worried about anything here?
         */
        @Override
        public A head() {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Cons.head is missing!");
        }

        @Override
        public SealedList<A> tail() {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Cons.tail is missing!");
        }

        /*
          Part 3:

          Add missing `toString` method for Cons case. The string should be
          built from elements:
          - "Cons("
          - stored element as string
          - tail converted to string
          - ")"
         */
        @Override
        public String toString() {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Cons.toString is missing!");
        }
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

    private SealedList() {
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