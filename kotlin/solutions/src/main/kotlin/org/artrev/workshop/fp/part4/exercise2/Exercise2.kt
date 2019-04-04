@file:Suppress("PackageName")

package org.artrev.workshop.fp.part4.exercise2

import java.io.File
import java.io.IOException
import java.util.concurrent.atomic.AtomicReference

/*
  - what kind of functions pure or impure can be memoized?
    Only pure, referential transparent functions!

  - why?
    Because any side effect that is done by an impure function will occur only
    once for the memoized version. For example no logging like in the `loadAccident`
    below.
 */
internal fun <A> (() -> A).memoized(): () -> A {
    val memo: AtomicReference<A> = AtomicReference()
    return {
        synchronized(memo) {
            if (memo.get() == null) memo.set(invoke())
            memo.get()
        }
    }
}


internal class AccidentMetadata(val id: Long, val source: File) {
    val data: () -> String = ::loadAccident.memoized()

    private fun loadAccident(): String {
        try {
            return source.readLines().reduce { a, b -> a + b }
        } catch (e: IOException) {
            throw SourceFileCorrupted("Error when reading: ${source.absolutePath}")
        }
    }

    class SourceFileCorrupted internal constructor(message: String) : RuntimeException(message)
}

fun main(args: Array<String>) {
    val file = File("src/main/resources/part4/accident.info")
    val metadata = AccidentMetadata(42L, file)

    // Let's see the console and check if the accident data is correctly
    // memoized
    var data = "error!"
    for (i in 0..99) {
        data = metadata.data()
    }

    println("accident data = $data")
}
