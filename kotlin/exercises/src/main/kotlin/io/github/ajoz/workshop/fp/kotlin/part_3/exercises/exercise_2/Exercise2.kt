@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_2

/*
  -- Creating a Boolean from scratch - vol. 2 --

  In the first exercise we created a Boolean with the use of the enum class.

  Enum was a very nice addition in Java 1.5 but also very limiting:
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
  could be:

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

  Are there any other sum types you know of?

  Char = a | b | c | d | ... | A | B | C | ...
  Natural = 0 | 1 | 2 | 3 | ...

  Let's rewrite the Boolean implementation this time without using an enum.
 */
sealed class SealedBoolean {
    object True : SealedBoolean() {
        /*
          Part 1:

          Add missing implementations for the True and False methods.
         */
        override fun and(other: SealedBoolean) =
                TODO("Exercise 2 True.and is missing!")

        override fun or(other: SealedBoolean) =
                TODO("Exercise 2 True.or is missing!")

        override operator fun not() =
                TODO("Exercise 2 True.not is missing!")

        override fun ifTrue(effect: () -> Unit) =
                TODO("Exercise 2 True.ifTrue is missing!")

        override fun ifFalse(effect: () -> Unit) =
                TODO("Exercise 2 True.ifFalse is missing!")

        override fun <A> match(ifTrue: () -> A, ifFalse: () -> A) =
                TODO("Exercise 2 True.match is missing!")

        override fun toString(): String {
            return "SealedTrue"
        }
    }

    object False : SealedBoolean() {

        override fun and(other: SealedBoolean) =
                TODO("Exercise 2 False.and is missing!")

        override fun or(other: SealedBoolean) =
                TODO("Exercise 2 False.or is missing!")

        override operator fun not() =
                TODO("Exercise 2 False.not is missing!")

        override fun ifTrue(effect: () -> Unit) =
                TODO("Exercise 2 False.ifTrue is missing!")

        override fun ifFalse(effect: () -> Unit) =
                TODO("Exercise 2 False.ifFalse is missing!")

        override fun <A> match(ifTrue: () -> A, ifFalse: () -> A) =
                TODO("Exercise 2 False.match is missing!")

        override fun toString(): String {
            return "SealedFalse"
        }
    }

    abstract infix fun and(other: SealedBoolean): SealedBoolean
    abstract infix fun or(other: SealedBoolean): SealedBoolean
    abstract operator fun not(): SealedBoolean

    abstract fun ifTrue(effect: () -> Unit): SealedBoolean
    abstract fun ifFalse(effect: () -> Unit): SealedBoolean

    abstract fun <A> match(ifTrue: () -> A, ifFalse: () -> A): A

    companion object {
        val TRUE: SealedBoolean = True
        val FALSE: SealedBoolean = False
    }
}

fun main(args: Array<String>) {
    // simple boolean algebra:
    // and:
    println(SealedBoolean.TRUE and SealedBoolean.TRUE)
    println(SealedBoolean.TRUE and SealedBoolean.FALSE)
    println(SealedBoolean.FALSE and SealedBoolean.FALSE)
    println(SealedBoolean.FALSE and SealedBoolean.TRUE)

    // or:
    println(SealedBoolean.TRUE or SealedBoolean.FALSE)
    println(SealedBoolean.FALSE or SealedBoolean.TRUE)
    println(SealedBoolean.FALSE or SealedBoolean.FALSE)
    println(SealedBoolean.TRUE or SealedBoolean.TRUE)

    // not:
    println(SealedBoolean.TRUE.not())
    println(SealedBoolean.FALSE.not())

    // conditions as methods:
    SealedBoolean.TRUE
            .ifTrue { println("SealedTrue is SealedTrue!") }
            .ifFalse { println("SealedTrue is SealedFalse?") }

    SealedBoolean.FALSE
            .ifFalse { println("SealedFalse is SealedFalse!") }
            .ifTrue { println("SealedFalse is SealedTrue?") }

    // conditions as expressions:
    val trueMessage = SealedBoolean.TRUE
            .match(
                    { "Matching SealedTrue to SealedTrue!" },
                    { "Matching SealedTrue to SealedFalse?" }
            )
    println(trueMessage)

    val falseMessage = SealedBoolean.FALSE
            .match(
                    { "Matching SealedFalse to SealedTrue?" },
                    { "Matching SealedFalse to SealedFalse!" }
            )
    println(falseMessage)

    val bool = SealedBoolean.FALSE
    when (bool) {
        SealedBoolean.FALSE -> println("Switch FALSE")
        SealedBoolean.TRUE -> println("Switch TRUE")
    }
}
