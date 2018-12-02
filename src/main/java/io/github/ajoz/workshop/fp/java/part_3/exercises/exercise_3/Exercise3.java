package io.github.ajoz.workshop.fp.java.part_3.exercises.exercise_3;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Function2;

import static io.github.ajoz.workshop.fp.java.part_3.exercises.exercise_3.SealedList.cons;
import static io.github.ajoz.workshop.fp.java.part_3.exercises.exercise_3.SealedList.nil;

/*
  -- Beyond Sum Types --

  In the previous exercises we created a Sum type. There is another kind of type
  out there called a Product type.

  Product type is a compound type that has a structure. The structure of the type
  is determined by the order of the operands (other types) in the product. Instance
  of a product type contains all possible instances of its primitive data types.

  A simple example of a product type is a Pair<A, B>. A Pair denotes a combination
  of all possible instances of A and B.

  Pair = A * B

  In Java one might say that every class is a possible product type if you squint
  your eyes a little.

  class User {
      String name;
      String surname;
      Integer age;
  }

  is just

  User = String * String * Integer

  Just by looking at the used notation (ML-like) you can understand why its called
  a product type.

  Types extending both Sum and Product types are called Algebraic Data Types.

  Now let's create an example.

  Imagine you are working for a package delivery company on their management
  system. In your API you have a class called PackageDeliveryStatus, this
  class describes the current status of a particular package:
  - its id
  - its tracking code (this is only valid for packages that are already dispatched)
  - its stage of processing
  - its cause of rejection (this is only valid for packages that are rejected,
    using an enum for it is causing us to create a silly RejectionCause.NO_CAUSE
    if we do not want to use a null)

  It is not that bad? Yes because we are used to this kind of classes. There are
  some unfortunate problems lurking in such code:
  - incorrect states are possible e.g. rejected package with a tracking code
  - we are not relaying on type system to prevent bugs
 */
@SuppressWarnings("unused")
class PackageDeliveryStatus {
    public Long id;
    public String trackingCode;
    public Stage stage;
    public RejectionCause rejectionCause;

    enum Stage {
        PREPARING,
        PREPARED,
        REJECTED,
        DISPATCHED,
        WAITING_FOR_RECIPIENT_TO_LEAVE_HOME_SO_HE_WILL_MISS_IT
    }

    enum RejectionCause {
        NO_CAUSE,
        RECIPIENT_UNKNOWN,
        PACKAGE_DAMAGED
    }
}

/*
  We can try to solve this with ADTs (Product and Sum Types). Let's write this
  type again (this time in F#):

  type RejectionCause = RECEPIENT_UNKNOWN | PACKAGE_DEMAGED

  type PackageDeliveryStatus =
        | Preparing of Long
        | Prepared of Long
        | Rejected of Long * RejectionCause
        | Dispatched of Long * String

  We create a sealed hierarchy of PackageDeliveryStatus with several subclasses:
  - Preparing that has only an ID of type Long
  - Prepared that has only and ID of type Long
  - Rejected that has an ID of type Long and RejectionCause. Please notice that
    we can now remove this silly RejectionCause.NO_CAUSE because the RejectionCause
    will never appear in any other case
  - Dispatched that has an ID of type Long and a TrackingStatus of type String.

  Let's rewrite it in Java
 */
@SuppressWarnings("unused")
abstract class PackageDeliveryStatus2 {
    private static class Preparing extends PackageDeliveryStatus2 {
        public Long id;
    }

    private static class Prepared extends PackageDeliveryStatus2 {
        public Long id;
    }

    private static class Rejected extends PackageDeliveryStatus2 {
        public Long id;
        public RejectionCause rejectionCause;
    }

    private static class Dispatched extends PackageDeliveryStatus2 {
        public Long id;
        public String trackingCode;
    }

    // id is a common thing between all of those subtypes so it could be
    // moved to the PackageDeliveryStatus2 class

    enum RejectionCause {
        RECIPIENT_UNKNOWN,
        PACKAGE_DAMAGED
    }

    private PackageDeliveryStatus2() {
    }
}

/*
  But how to work with types?

  We are taught that instanceOf is bad because of the performance. On the other
  hand creating methods isPreparing(), isPrepared(), isDispatched() ... and
  casting to correct type sounds like a hassle.

  The answer lies in polymorphism, subtype polymorphism to be more precise.
  Remember how we implemented the `ifTrue`, `ifFalse` and `match` methods?

  Working with such sealed hierarchies in Java can be solved in similar manner.

  Let's practice a bit creating and working with ADTs then.

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
@SuppressWarnings("WeakerAccess")
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

        /*
          Part 2:

          Add missing `map` and `foldLeft` methods for Nil case.

          Questions:
          - what should map on an empty List return?
          - what should foldLeft on an empty List return?
         */
        @Override
        public <B> SealedList<B> map(final Function1<A, B> mapper) {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Nil.tail is missing!");
        }

        @Override
        public <B> B foldLeft(final B initial,
                              final Function2<B, A, B> operator) {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Nil.foldLeft is missing!");
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
          Part 3:

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
          Part 4:

          Add missing `map` and `foldLeft` methods for Cons case.

          This is a tricky part. The SealedList is a recursive type and thus it
          needs recursion to perform `map` and `foldLeft`.

          If you do not want, you don't have to express `map` in terms of
          `foldLeft`.

          Hints:
          - take a look at the at the SealedList.cons method
          - think how the list is created
          - map boils down to creating new list with changed values
         */
        @Override
        public <B> SealedList<B> map(final Function1<A, B> mapper) {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Cons.map is missing!");
        }

        /*
          In case of such recursive type `foldLeft` is a different beast. What
          was easy as a simple iteration needs to be expressed as a recursion.

          Let's quickly examine the folding. Let's write the `foldLeft` as

          foldLeft :: (b -> a -> b) -> b -> List a -> b

          The function takes two argument function (operator - op in short),
          initial value, list that is going to be folded and returns a value.

          In case the list is empty (Nil case) we should return the initial
          value, this is pretty simple.

          foldLeft op initial Nil = initial

          What about any other case? (Cons). Let's look at the foldLeft then:

          foldLeft op initial (Cons head tail) = ????

          We need to use operator on the initial value and the head value, so:

          foldLeft op initial (Cons head tail) = initial op head

          This is not yet the thing that we want to achieve as we need to fold
          the tail.

          In our imperative approach we had an accumulator and at the begining
          it had the initial value:

          B accumulator = initial;
          for (final A element : list) {
              accumulator = operator.apply(accumulator, element);
          }

          then we overwrote it with the result of the operator. One could say that
          the new computed value became the initial value for the next iteration.
          This next iteration was done on the rest of the list -- the tail.

          Let's use this knowledge:

          foldLeft op initial (Cons head tail) = foldLeft op (initial op head) tail

          Let's try to do the same in Java (in OO-ish way)

          Hint:
          - on what thing the `foldLeft` will be invoked?
          - the same operator is passed in each iteration
         */
        @Override
        public <B> B foldLeft(final B initial,
                              final Function2<B, A, B> operator) {
            throw new UnsupportedOperationException("Exercise 3 SealedList.Cons.foldLeft is missing!");
        }

        @Override
        public String toString() {
            return String.format("Cons(%s, %s)", head, tail.toString());
        }
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

    private SealedList() {
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
