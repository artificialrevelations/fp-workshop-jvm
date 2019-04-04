@file:Suppress("PackageName", "UnusedMainParameter")

package org.artrev.workshop.fp.part1.exercise6

// Part 1:
infix fun <A, B> ((B) -> Unit).compose(function: (A) -> B): (A) -> Unit = { a: A ->
    this(function(a))
}

// Part 2:
internal object ComposingConsumer {
    @JvmStatic
    fun main(args: Array<String>) {
        val printInt: (Int) -> Unit = { println(it) }
        val strlen: (String) -> Int = { it.length }

        val printStrLen = printInt.compose(strlen)

        printStrLen("https://www.meetup.com/Java-User-Group-Lodz/")
    }
}

// Part 3:
infix fun <A, B> (() -> A).andThen(function: (A) -> B): () -> B = {
    function(this())
}

// Part 4:
internal object ComposingSupplier {
    @JvmStatic
    fun main(args: Array<String>) {
        val getFacebook = { "https://www.facebook.com/groups/juglodz/" }
        val strlen: (String) -> Int = { it.length }

        val getFBLen = getFacebook.andThen(strlen)

        println(getFBLen())
    }
}

// Part 5:
fun <A, B, C> ((A) -> (B) -> C).applyFirst(supplier: () -> A): (B) -> C = { b: B ->
    this(supplier())(b)
}

// Part 6:
fun <A, B, C> ((A) -> (B) -> C).applySecond(supplier: () -> B): (A) -> C = { a: A ->
    this(a)(supplier())
}