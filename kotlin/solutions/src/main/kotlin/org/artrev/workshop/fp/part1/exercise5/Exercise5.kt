@file:Suppress("unused", "UNUSED_PARAMETER", "UNUSED_VARIABLE", "UnusedMainParameter")

package org.artrev.workshop.fp.part1.exercise5

import org.artrev.workshop.fp.part1.exercise3.andThen

// Part 1
fun <A, B, C> ((A) -> (B) -> C).applyFirst(value: A): (B) -> C =
        { b -> this(value)(b) }

// Part 2
fun <A, B, C> ((A) -> (B) -> C).applySecond(value: B): (A) -> C =
        { a -> this(a)(value) }

object Part3 {
    @JvmStatic
    fun main(args: Array<String>) {
        val addInts: (Int) -> (Int) -> Int = { a -> { b -> a + b } }

        val addOne: (Int) -> Int = addInts(1)

        println(addOne(0))
        println(addOne(1))
        println(addOne(41))

        // Partially apply `concatStrings` so you will get as a result a function
        val concatStrings: (String) -> (String) -> String =
                { first -> { second -> first + second } }

        // that adds a "foo" prefix to any String argument
        val fooPrefix: (String) -> String =
                concatStrings("foo")

        // that adds a "bar" suffix to any String argument
        val barSuffix: (String) -> String =
                concatStrings.applySecond("bar")

        println(fooPrefix("rever with JUG Łódź!"))
        println(barSuffix("Unfortunately no sponsors for an open "))
    }
}

// Part 4
fun <A, B, C> ((A, B) -> C).applyFirst(value: A): (B) -> C =
        { b -> this(value, b) }

// Part 5
fun <A, B, C> ((A, B) -> C).applySecond(value: B): (A) -> C =
        { a -> this(a, value) }

object Part6 {
    @JvmStatic
    fun main(args: Array<String>) {
        // this function drops a certain amount of chars from a given string
        val drop: (String, Int) -> String = { string, amount ->
            val length = string.length
            if (amount < length)
                string.substring(amount, length)
            else
                ""
        }

        val length: (String) -> Int = { it.length }

        // We want to compose function `drop` with function `length`
        // We always want to drop 6 first characters from a given string

        val substrlen: (String) -> Int =
                drop.applySecond(6).andThen(length)

        println(substrlen("This exercise is very easy?"))
    }
}
