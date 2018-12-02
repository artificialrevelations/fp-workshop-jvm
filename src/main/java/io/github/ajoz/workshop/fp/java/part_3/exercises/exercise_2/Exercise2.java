package io.github.ajoz.workshop.fp.java.part_3.exercises.exercise_2;

import io.github.ajoz.workshop.fp.java.tools.Supplier;

/*
  -- Creating a Boolean from scratch - vol. 2 --

  In the first exercise we created a Boolean with the use of the enum class.

  Enum was a very nice addition in Java 1.5 but is also very limiting:
  - there is only one instance of each enum case
  - although its possible to define multiple constructors for an enum, they can
    only be used in place of case definition

    Its not possible to have:

    enum Foo {
        BAR,
        BAZ;

        Foo(String p) {}
    }

    Foo foo = new Foo.Bar("String defined in place");
  - enum is very nice for expressing simple sum types.

  What is a sum type?

  It is a thing that can be found in different programming languages:
  - tagged union
  - variant
  - variant record
  - choice type
  - discriminated union
  - disjoint union

  More precisely it is a data structure used to hold a value, that could take
  on several distinct (fixed) types. The most simple implementation of such type
  could be

  class IntOrBoolean {
      Integer integer;
      Boolean boolean;
      boolean isInteger; // the so called tag we need to check before access
  }

  final IntOrBoolean union = new IntOrBoolean(42);

  if (union.isInteger) {
      System.out.println("This union holds an Int!");
  } else {
      System.out.println("This union holds a Boolean!");
  }

  So why is it called a sum type? Let's look at a more concise Boolean
  definition:

  data Bool = True | False

  What do we see here? A Boolean is a True or False. We can think that Boolean
  is a type that is a sum of True and False if we would like to count how many
  different distinct values it can hold.

  Boolean = True + False

  As both True and False does not hold any other values we can substitute them
  with 1's.

  Boolean = 1 + 1

  Are there any other sum types you know of?

  Char = a | b | c | d | ... | A | B | C | ...
  Natural = 0 | 1 | 2 | 3 | ...

  From the previous example:

  IntOrBoolean = IntCase | BooleanCase

  Let's rewrite the Boolean implementation this time without using an enum.
 */
@SuppressWarnings({"WeakerAccess", "StaticInitializerReferencesSubClass"})
abstract class SealedBoolean {
    private SealedBoolean() {
        // abstract class with a private constructor allows us to create a
        // sealed class hierarchy. This private constructor will be only
        // accessible to private classes (static or not) of the abstract class.

        // This gives us power as we can have an abstract type and we control
        // how many subtypes of it exist.
    }

    public static class True extends SealedBoolean {
        /*
          Part 1:

          Add missing implementations for the True and False methods.
         */
        @Override
        public SealedBoolean and(final SealedBoolean other) {
            throw new UnsupportedOperationException("Exercise 2 True.and is missing!");
        }

        @Override
        public SealedBoolean or(final SealedBoolean other) {
            throw new UnsupportedOperationException("Exercise 2 True.or is missing!");
        }

        @Override
        public SealedBoolean not() {
            throw new UnsupportedOperationException("Exercise 2 True.not is missing!");
        }

        @Override
        public SealedBoolean ifTrue(final Runnable effect) {
            throw new UnsupportedOperationException("Exercise 2 True.ifTrue is missing!");
        }

        @Override
        public SealedBoolean ifFalse(final Runnable effect) {
            throw new UnsupportedOperationException("Exercise 2 True.ifFalse is missing!");
        }

        @Override
        public <A> A match(final Supplier<A> ifTrue,
                           final Supplier<A> ifFalse) {
            throw new UnsupportedOperationException("Exercise 2 True.match is missing!");
        }

        @Override
        public String toString() {
            return "SealedTrue";
        }
    }

    public static class False extends SealedBoolean {

        @Override
        public SealedBoolean and(final SealedBoolean other) {
            throw new UnsupportedOperationException("Exercise 2 False.and is missing!");
        }

        @Override
        public SealedBoolean or(final SealedBoolean other) {
            throw new UnsupportedOperationException("Exercise 2 False.or is missing!");
        }

        @Override
        public SealedBoolean not() {
            throw new UnsupportedOperationException("Exercise 2 False.not is missing!");
        }

        @Override
        public SealedBoolean ifTrue(final Runnable effect) {
            throw new UnsupportedOperationException("Exercise 2 False.ifTrue is missing!");
        }

        @Override
        public SealedBoolean ifFalse(final Runnable effect) {
            throw new UnsupportedOperationException("Exercise 2 False.ifFalse is missing!");
        }

        @Override
        public <A> A match(final Supplier<A> ifTrue,
                           final Supplier<A> ifFalse) {
            throw new UnsupportedOperationException("Exercise 2 False.match is missing!");
        }

        @Override
        public String toString() {
            return "SealedFalse";
        }
    }

    public abstract SealedBoolean and(final SealedBoolean other);

    public abstract SealedBoolean or(final SealedBoolean other);

    public abstract SealedBoolean not();

    public abstract SealedBoolean ifTrue(final Runnable effect);

    public abstract SealedBoolean ifFalse(final Runnable effect);

    public abstract <A> A match(final Supplier<A> ifTrue,
                                final Supplier<A> ifFalse);

    public static final SealedBoolean TRUE = new True();
    public static final SealedBoolean FALSE = new False();
}

class Exercise2 {
    public static void main(final String[] args) {
        // simple boolean algebra:
        // and:
        System.out.println(SealedBoolean.TRUE.and(SealedBoolean.TRUE));
        System.out.println(SealedBoolean.TRUE.and(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.FALSE.and(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.FALSE.and(SealedBoolean.TRUE));

        // or:
        System.out.println(SealedBoolean.TRUE.or(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.FALSE.or(SealedBoolean.TRUE));
        System.out.println(SealedBoolean.FALSE.or(SealedBoolean.FALSE));
        System.out.println(SealedBoolean.TRUE.or(SealedBoolean.TRUE));

        // not:
        System.out.println(SealedBoolean.TRUE.not());
        System.out.println(SealedBoolean.FALSE.not());

        // conditions as methods:
        SealedBoolean.TRUE
                .ifTrue(() -> System.out.println("SealedTrue is SealedTrue!"))
                .ifFalse(() -> System.out.println("SealedTrue is SealedFalse?"));

        SealedBoolean.FALSE
                .ifFalse(() -> System.out.println("SealedFalse is SealedFalse!"))
                .ifTrue(() -> System.out.println("SealedFalse is SealedTrue?"));

        // conditions as expressions:
        final String trueMessage =
                SealedBoolean.TRUE
                        .match(
                                () -> "Matching SealedTrue to SealedTrue!",
                                () -> "Matching SealedTrue to SealedFalse?"
                        );
        System.out.println(trueMessage);

        final String falseMessage =
                SealedBoolean.FALSE
                        .match(
                                () -> "Matching SealedFalse to SealedTrue?",
                                () -> "Matching SealedFalse to SealedFalse!"
                        );
        System.out.println(falseMessage);

        // using the switch on Strings (only Java 7+) - super wonky:
        final SealedBoolean bool = SealedBoolean.FALSE;
        switch (bool.toString()) {
            // we cannot do
            // case SealedBoolean.FALSE.toString()
            // because switch statement demands constant conditions :-(
            case "SealedFalse":
                System.out.println("Switch FALSE");
                break;
            case "SealedTrue":
                System.out.println("Switch TRUE");
                break;
        }

        // Is using instanceOf really that bad?
        if (bool instanceof SealedBoolean.True) {
            System.out.println("InstanceOf TRUE");
        } else {
            System.out.println("InstanceOf FALSE");
        }
    }
}
