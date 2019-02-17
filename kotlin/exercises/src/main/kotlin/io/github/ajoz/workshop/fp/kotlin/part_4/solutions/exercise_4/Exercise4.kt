@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_4.solutions.exercise_4

import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.util.*
import java.util.concurrent.atomic.AtomicReference

internal fun <A, B> (() -> A).map(function: (A) -> B): () -> B = {
    function(this())
}

internal fun <A, B> (() -> A).flatMap(function: (A) -> () -> B): () -> B = {
    function(this())()
}

internal fun <A> (() -> A).before(effect: () -> Unit): () -> A = {
    effect()
    this()
}

internal fun <A> (() -> A).after(effect: (A) -> Unit): () -> A = {
    val result = this()
    effect(result)
    result
}

internal fun <A> (() -> A).memoized(): () -> A {
    val memo: AtomicReference<A> = AtomicReference()
    return {
        synchronized(memo) {
            if (memo.get() == null) memo.set(invoke())
            memo.get()
        }
    }
}

internal data class Accident(
        val latitude: Double,
        val longitude: Double,
        val message: String
)

internal class AccidentMetadata(val id: Long, val source: File) {
    val data: () -> Accident = ::loadAccident
            // before the loading print logs to console
            .before { println("Loading Accident id = $id") }
            // parse everything
            .map { dataToRecord(it) }
            // print the lines
            .after { printRecord(it) }
            // transform the record
            .map { recordToAccident(it) }
            // print the result
            .after { println(it) }
            .memoized()

    // We should strive to avoid bare lambdas and upgrade them to named
    // functions
    private fun recordToAccident(record: Array<String>): Accident {
        val latitude = java.lang.Double.valueOf(record[0])
        val longitude = java.lang.Double.valueOf(record[1])
        val message = record[2]
        return Accident(latitude, longitude, message)
    }

    private fun printRecord(record: Array<String>) {
        println(Arrays.toString(record))
    }

    // This could probably moved out of this class into something more suitable:
    // a separate Type for the result of parsing
    private fun dataToRecord(data: String): Array<String> {
        return data.split(",".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()
    }

    private fun loadAccident(): String {
        try {
            val path = source.toPath()
            val bytes = Files.readAllBytes(path)
            return String(bytes)
        } catch (e: IOException) {
            throw SourceFileCorrupted("Error when reading: ${source.absolutePath}")
        }
    }

    class SourceFileCorrupted internal constructor(message: String) : RuntimeException(message)
}

fun main(args: Array<String>) {
    val file = File("src/main/resources/part_4/accident.info")
    val metadata = AccidentMetadata(42L, file)

    // Let's see the console and check if the accident data is correctly
    // memoized
    var data: Accident? = null
    for (i in 0..99) {
        data = metadata.data()
    }

    println("accident data = " + data!!)
}
