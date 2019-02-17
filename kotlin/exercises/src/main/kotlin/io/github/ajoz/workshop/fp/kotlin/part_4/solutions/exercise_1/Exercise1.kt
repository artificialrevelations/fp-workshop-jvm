@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_4.solutions.exercise_1

import java.io.File
import java.io.IOException

/*
  The order of the fields and methods in this file was changed to better show
  the changes that were made.

  - Did we achieve the laziness?
    Yes! and the class is fully immutable, much less problematic to debug or work
    with.
  - Are there any problems with the implementation?
    Yes :-( The code "wrapped" by the supplier is invoked each time the `getData`
    is called.
 */
// Change #1: We make the existing properties immutable
internal class AccidentMetadata(val id: Long, private val source: File) {
    // Change #2: We change the property to be readonly
    val data: () -> String = ::loadAccident

    // Change #3: We change the return type of the `loadAccident` method from Unit
    //            to String. We make the method private.
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
    // configuration:
    val file = File("src/main/resources/part_4/accident.info")

    // class creation:
    val metadata = AccidentMetadata(42L, file)

    // property usage
    metadata.data().let(::println)
}
