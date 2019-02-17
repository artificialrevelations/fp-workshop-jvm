@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_1

/*
  -- Creating a Boolean type from scratch - vol. 1 --

  In the first part of the workshop we were looking at an example from the
  language Smalltalk. It showed that a programming language does not need to
  have a built-in concept of a boolean. In Smalltalk both True and False
  were objects that could receive messages.

  Let's try to reimplement this in Kotlin.

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

  Enough of this philosophy, how to define it in Kotlin?

  A natural thing is to use an enum class. Kotlin allows creating type-safe
  enums consisting of constants separated by a comma.

  A simple Boolean enum class might look like:

  enum class EnumBoolean {
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

  Should we store the "real" Kotlin boolean inside? :-)
  Should we add a nice `toBoolean()` method so we can use `ifs`? :-)

  Maybe we will manage to do something without resorting to such solutions. First
  let's think about `and`, `or`, `not`. We could add those missing methods
  to the enum class , but this would mean using a lot of switch statements e.g.:

  enum class EnumBoolean {
       TRUE,
       FALSE;

       fun and(other: Foo) = when (this) {
           TRUE -> other
           FALSE -> this
       }
  }

  We have another option. Java Language Specification gives us a hint:

  "An enum declaration is implicitly final unless it contains at least one enum
  constant that has a class body"

  Kotlin is no different and allows for the same but in much more concise manner.

  Let's do this then!
 */
enum class EnumBoolean {
    TRUE {
        /*
          Part 1:

          Consider this TRUE case and implement methods: `and`, `or`, `not`.
         */
        override fun and(other: EnumBoolean) =
                TODO("Exercise 1 TRUE.and is missing!")

        override fun or(other: EnumBoolean) =
                TODO("Exercise 1 TRUE.or is missing!")

        override operator fun not() =
                TODO("Exercise 1 TRUE.not is missing!")

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

          Please add implementation of `ifTrue` and `ifFalse`
         */
        override fun ifTrue(effect: () -> Unit) =
                TODO("Exercise 1 TRUE.ifTrue is missing!")

        override fun ifFalse(effect: () -> Unit) =
                TODO("Exercise 1 TRUE.ifFalse is missing!")

        /*
          Part 3:

          Kotlin treats `if`s as expressions and we would like to have the same
          ability with out newly created type.

          We can define a `match` method that returns some type A and takes
          two different Suppliers one for TRUE case and second for FALSE.

          Passed suppliers should be called in the correct case.

          Please add implementation of `match` method.
         */
        override fun <A> match(ifTrue: () -> A, ifFalse: () -> A): A =
                TODO("Exercise 1 TRUE.match is missing!")
    },
    FALSE {
        /*
          Part 4:

          Please add implementations of missing methods for the FALSE case.

          Questions:
          - Can we model anything useful with enum class?
          - What are the issues with enum class?
          - Can each case take different arguments?
          - Can we instantiate the same case with different arguments?
         */
        override fun and(other: EnumBoolean) =
                TODO("Exercise 1 FALSE.and is missing!")

        override fun or(other: EnumBoolean) =
                TODO("Exercise 1 FALSE.or is missing!")

        override operator fun not() =
                TODO("Exercise 1 FALSE.not is missing!")

        override fun ifTrue(effect: () -> Unit) =
                TODO("Exercise 1 FALSE.ifTrue is missing!")

        override fun ifFalse(effect: () -> Unit) =
                TODO("Exercise 1 FALSE.ifFalse is missing!")

        override fun <A> match(ifTrue: () -> A, ifFalse: () -> A) =
                TODO("Exercise 1 FALSE.match is missing!")
    };

    /*
      Added benefit of Kotlin (comparing to Java) is the possibility of marking
      the classes with infix keyword.
     */
    abstract infix fun and(other: EnumBoolean): EnumBoolean

    abstract infix fun or(other: EnumBoolean): EnumBoolean

    abstract operator fun not(): EnumBoolean

    abstract fun ifTrue(effect: () -> Unit): EnumBoolean

    abstract fun ifFalse(effect: () -> Unit): EnumBoolean

    abstract fun <A> match(ifTrue: () -> A, ifFalse: () -> A): A
}

fun main(args: Array<String>) {
    // simple boolean algebra:
    // and:
    println(EnumBoolean.TRUE and EnumBoolean.TRUE)
    println(EnumBoolean.TRUE and EnumBoolean.FALSE)
    println(EnumBoolean.FALSE and EnumBoolean.FALSE)
    println(EnumBoolean.FALSE and EnumBoolean.TRUE)

    // or:
    println(EnumBoolean.TRUE or EnumBoolean.FALSE)
    println(EnumBoolean.FALSE or EnumBoolean.TRUE)
    println(EnumBoolean.FALSE or EnumBoolean.FALSE)
    println(EnumBoolean.TRUE or EnumBoolean.TRUE)

    // not:
    println(EnumBoolean.TRUE.not())
    println(EnumBoolean.FALSE.not())

    // conditions as methods:
    EnumBoolean.TRUE
            .ifTrue { println("EnumTrue is EnumTrue!") }
            .ifFalse { println("EnumTrue is EnumFalse?") }

    EnumBoolean.FALSE
            .ifFalse { println("EnumFalse is EnumFalse!") }
            .ifTrue { println("EnumFalse is EnumTrue?") }

    // conditions as expressions:
    val trueMessage = EnumBoolean.TRUE
            .match(
                    { "Matching EnumTrue to EnumTrue!" },
                    { "Matching EnumTrue to EnumFalse?" }
            )
    println(trueMessage)

    val falseMessage = EnumBoolean.FALSE
            .match(
                    { "Matching EnumFalse to EnumTrue?" },
                    { "Matching EnumFalse to EnumFalse!" }
            )
    println(falseMessage)

    // using when:
    val bool = EnumBoolean.TRUE
    when (bool) {
        EnumBoolean.FALSE -> println("When FALSE")
        EnumBoolean.TRUE -> println("When TRUE")
    }
}

