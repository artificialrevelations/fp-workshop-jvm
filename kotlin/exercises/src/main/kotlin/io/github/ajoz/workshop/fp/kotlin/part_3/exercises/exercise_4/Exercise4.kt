@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_4

import io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_4.SealedList.Cons
import io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_4.SealedList.Nil

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
sealed class SealedList<out A> {
    object Nil : SealedList<Nothing>() {
        /*
          Part 1:

          Add missing `head` and `tail` methods for Nil case.

          Head of the list is the first element of the list.
          In case of an empty List (Nil) a NoSuchElementException should be thrown.

          Tail of the list is all the elements except the head.
          In case of an empty List (Nil) a NoSuchElementException should be thrown.
         */
        override fun head() =
                TODO("Exercise 3 SealedList.Nil.head is missing!")

        override fun tail() =
                TODO("Exercise 3 SealedList.Nil.tail is missing!")

        override fun toString() = "Nil"
    }

    class Cons<A>(val head: A, val tail: SealedList<A>) : SealedList<A>() {
        /*
          Part 2:

          Add missing `head` and `tail` methods for Cons case.

          Questions:
          - should we be worried about anything here?
         */
        override fun head() =
                TODO("Exercise 3 SealedList.Cons.head is missing!")

        override fun tail() =
                TODO("Exercise 3 SealedList.Cons.tail is missing!")

        /*
          Part 3:

          Add missing `toString` method for Cons case. The string should be
          built from elements:
          - "Cons("
          - stored element as string
          - tail converted to string
          - ")"
         */
        override fun toString() =
                TODO("Exercise 3 SealedList.Cons.toString is missing!")
    }

    abstract fun head(): A
    abstract fun tail(): SealedList<A>
}

fun main(args: Array<String>) {
    // A new list is created
    val sealedList = Cons(1, Cons(2, Cons(3, Nil)))

    println("sealedList = $sealedList")
}