package io.github.ajoz.workshop.fp.java.part3.exercise1;

import io.github.ajoz.workshop.fp.java.tools.Effect;
import io.github.ajoz.workshop.fp.java.tools.Supplier;

/*
  -- Creating a Boolean type from scratch - vol. 1 --

  In the first part of the workshop we were looking at an example from the
  language Smalltalk. It showed that a programming language does not need to
  have a built-in concept of a boolean. In Smalltalk both True and False
  were objects that could receive messages.

  Let's try to reimplement this in Java.

  First let's define what a Boolean is. Boolean is a type that has only two
  possible values denoting truth and false (from logic and Boolean algebra).

  data Boolean = True | False

  This concise notation gets to the point: A Boolean is a data type that is
  either True or False. The True and False are - one could say - "atomic" and
  cannot be deconstructed further into other types. They are also not wrapping
  anything they just are!

  There is also a sad truth lurking here, the two cases we just defined above
  do not have meaning, the meaning is given by us through functions we define
  on them like: `and`, `or`, `not`.

  We could have a different data type called Foo with two cases Bar and Baz
  that could work similarly.

  Enough of this philosophy, how to define it in Java?

  A natural thing is to use an enum. A quote from the official Java lang tutorial

  "An enum type is a special data type that enables for a variable to be a set
  of predefined constants."

  This is exactly what we want to achieve: a type with two predefined constants.

  A simple Boolean enum might look like:

  enum EnumBoolean {
       TRUE,
       FALSE
  }

  Almost as concise as the Haskell definition but what about the methods like:
  - and
  - or
  - not
  - xor
  - nand

  And most importantly what about `if` statements/expressions?

  Should we store the "real" Java boolean inside? :-)
  Should we add a nice `toBoolean()` method so we can use `ifs`? :-)

  Maybe we will manage to do something without resorting to such solutions. First
  let's think about `and`, `or`, `not`. We could add those missing methods
  to the enum, but this would mean using a lot of switch statements e.g.:

  enum EnumBoolean {
       TRUE,
       FALSE;

       public EnumBoolean and(final EnumBoolean other) {
           switch (this) {
               case TRUE: return other;
               case FALSE: return this;
           }
           throw new IllegalStateException("Boolean has more then two cases?: " + this);
       }
  }

  We have another option. Java Language Specification gives us a hint:

  "An enum declaration is implicitly final unless it contains at least one enum
  constant that has a class body"

  Let's do this instead!
 */
enum EnumBoolean {
    TRUE() {
        /*
          Part 1:

          Consider this TRUE case and implement methods: `and`, `or`, `not`.
         */
        @Override
        public EnumBoolean and(final EnumBoolean other) {
            throw new UnsupportedOperationException("Exercise 1 TRUE.and is missing!");
        }

        @Override
        public EnumBoolean or(final EnumBoolean other) {
            throw new UnsupportedOperationException("Exercise 1 TRUE.or is missing!");
        }

        @Override
        public EnumBoolean not() {
            throw new UnsupportedOperationException("Exercise 1 TRUE.not is missing!");
        }

        /*
          Part 2:

          The Smalltalk objects representing True and False support two distinct
          messages called `ifTrue` and `ifFalse`. These methods can handle what
          would normally be done with

          if (condition) {
              // if(true) block of code
          } else {
              // if(false) block of code
          }

          To simulate the block of code that is run in both cases we need a way
          of passing it to the function. We want something that does not take
          an argument and does not return a result.

          There is a type already that meets these expectations, it's as old as
          Java itself. It is called Runnable. Unfortunately it is associated
          with Threads and concurrency thus could spawn unnecessary confusion.

          This is why we can define something similar.

          interface Effect {
              void perform();
          }

          Please add implementation of `ifTrue` and `ifFalse`
         */
        @Override
        public EnumBoolean ifTrue(final Effect effect) {
            throw new UnsupportedOperationException("Exercise 1 TRUE.ifTrue is missing!");
        }

        @Override
        public EnumBoolean ifFalse(final Effect effect) {
            throw new UnsupportedOperationException("Exercise 1 TRUE.ifFalse is missing!");
        }

        /*
          Part 3:

          Java treats `if` as statement but it's much more useful as an
          expression. Expression allows us to pass the result from the `if` into
          another variable, this means that it could be final (immutable).

          We can define a `match` method that returns some type A and takes
          two different Suppliers one for TRUE case and second for FALSE.

          Passed suppliers should be called in the correct case.

          Please add implementation of `match` method.
         */
        @Override
        public <A> A match(final Supplier<A> ifTrue,
                           final Supplier<A> ifFalse) {
            throw new UnsupportedOperationException("Exercise 1 TRUE.match is missing!");
        }
    },
    FALSE() {
        /*
          Part 4:

          Please add implementations of missing methods for the FALSE case.

          Questions:
          - Can we model anything useful with enum?
          - What are the issues with enum?
          - Can each case take different arguments?
          - Can we instantiate the same case with different arguments?
         */
        @Override
        public EnumBoolean and(final EnumBoolean other) {
            throw new UnsupportedOperationException("Exercise 1 FALSE.and is missing!");
        }

        @Override
        public EnumBoolean or(final EnumBoolean other) {
            throw new UnsupportedOperationException("Exercise 1 FALSE.or is missing!");
        }

        @Override
        public EnumBoolean not() {
            throw new UnsupportedOperationException("Exercise 1 FALSE.not is missing!");
        }

        @Override
        public EnumBoolean ifTrue(final Effect effect) {
            throw new UnsupportedOperationException("Exercise 1 FALSE.ifTrue is missing!");
        }

        @Override
        public EnumBoolean ifFalse(final Effect effect) {
            throw new UnsupportedOperationException("Exercise 1 FALSE.ifFalse is missing!");
        }

        @Override
        public <A> A match(final Supplier<A> ifTrue,
                           final Supplier<A> ifFalse) {
            throw new UnsupportedOperationException("Exercise 1 FALSE.match is missing!");
        }
    };

    public abstract EnumBoolean and(final EnumBoolean other);

    public abstract EnumBoolean or(final EnumBoolean other);

    public abstract EnumBoolean not();

    public abstract EnumBoolean ifTrue(final Effect effect);

    public abstract EnumBoolean ifFalse(final Effect effect);

    public abstract <A> A match(final Supplier<A> ifTrue,
                                final Supplier<A> ifFalse);
}

@SuppressWarnings("ConstantConditions")
class Exercise1 {
    public static void main(String[] args) {
        // simple boolean algebra:
        // and:
        System.out.println(EnumBoolean.TRUE.and(EnumBoolean.TRUE));
        System.out.println(EnumBoolean.TRUE.and(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.FALSE.and(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.FALSE.and(EnumBoolean.TRUE));

        // or:
        System.out.println(EnumBoolean.TRUE.or(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.FALSE.or(EnumBoolean.TRUE));
        System.out.println(EnumBoolean.FALSE.or(EnumBoolean.FALSE));
        System.out.println(EnumBoolean.TRUE.or(EnumBoolean.TRUE));

        // not:
        System.out.println(EnumBoolean.TRUE.not());
        System.out.println(EnumBoolean.FALSE.not());

        // conditions as methods:
        EnumBoolean.TRUE
                .ifTrue(() -> System.out.println("EnumTrue is EnumTrue!"))
                .ifFalse(() -> System.out.println("EnumTrue is EnumFalse?"));

        EnumBoolean.FALSE
                .ifFalse(() -> System.out.println("EnumFalse is EnumFalse!"))
                .ifTrue(() -> System.out.println("EnumFalse is EnumTrue?"));

        // conditions as expressions:
        final String trueMessage =
                EnumBoolean.TRUE
                        .match(
                                () -> "Matching EnumTrue to EnumTrue!",
                                () -> "Matching EnumTrue to EnumFalse?"
                        );
        System.out.println(trueMessage);

        final String falseMessage =
                EnumBoolean.FALSE
                        .match(
                                () -> "Matching EnumFalse to EnumTrue?",
                                () -> "Matching EnumFalse to EnumFalse!"
                        );
        System.out.println(falseMessage);

        // using the switch:
        final EnumBoolean bool = EnumBoolean.TRUE;
        switch (bool) {
            case FALSE:
                System.out.println("Switch FALSE");
                break;
            case TRUE:
                System.out.println("Switch TRUE");
                break;
        }
    }
}
