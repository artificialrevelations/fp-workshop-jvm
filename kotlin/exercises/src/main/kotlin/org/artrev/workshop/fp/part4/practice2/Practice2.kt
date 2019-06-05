@file:Suppress("PackageName")

package org.artrev.workshop.fp.part4.practice2

import org.artrev.workshop.fp.tools.map
import org.artrev.workshop.fp.tools.memoized
import java.io.File
import java.io.IOException
import java.net.URL
import java.nio.file.Files

/*
  -- Illegal States of Accident Metadata --

  Your new API for AccidentMetadata is a success. Everyone in the "THERE" company
  is super happy with it until suddenly someone from QA reports that the app has
  problems on low-end devices with little to not storage space.

  This is a problem because Management has plans to storm the low-end devices
  market. You and your team are tasked with improving the AccidentMetadata API
  with capability of loading Accident information from a given URL.

  As you are very preoccupied with other things in the moment, someone from your
  team gets to do it. The problem is that he is not as proficient with design
  as you.

  After few days he comes back with the class below, although it is working it
  allows for illegal states to happen.

  The API can now be used to create a metadata that both has the file and url
  set to null. This is why a new exception was created called: MetadataCorrupted.
  The `loadAccident` method is getting complicated and long.

  You got a new requirement that the `getData` should be safe to use, so this
  means that no RuntimeExceptions should be thrown.

  Please rework the `AccidentMetadata`:
  - illegal states should not be representable
  - `getData` should return Try<A> instead of a bare A
  - loading the accident data should now throw checked exceptions

  Hints:
  - we do not need to store the File and URL as Maybe<File> or Maybe<URL> as this
    would cause the `loadAccident` to be as complex as it previously was
  - in the previous part we learned about modeling with types, can we use the
    knowledge here?
 */
internal class AccidentMetadata {
    val id: Long
    private val file: File?
    private val url: URL?

    constructor(id: Long, file: File) {
        this.id = id
        this.file = file
        this.url = null
    }

    constructor(id: Long, url: URL) {
        this.id = id
        this.file = null
        this.url = url
    }

    val data = ::loadAccident
            .map { dataToRecord(it) }
            .map { recordToAccident(it) }
            .memoized()

    private fun loadAccident(): String {
        if (null != file) {
            try {
                val path = file.toPath()
                val bytes = Files.readAllBytes(path)
                return String(bytes)
            } catch (e: IOException) {
                throw FileCorrupted("Error when reading: ${file.absolutePath}")
            }
        }

        if (null != url) {
            try {
                return url
                        .openConnection()
                        .getInputStream()
                        .use {
                            String(it.readBytes())
                        }
            } catch (exception: IOException) {
                throw URLCorrupted("Error while reading URL: ${url.path}")
            }
        }

        throw MetadataCorrupted("Both File and URL are null!")
    }

    class FileCorrupted internal constructor(message: String) : RuntimeException(message)
    class URLCorrupted internal constructor(message: String) : RuntimeException(message)
    class MetadataCorrupted internal constructor(message: String) : RuntimeException(message)

    private fun recordToAccident(record: Array<String>): Accident {
        val latitude = java.lang.Double.valueOf(record[0])
        val longitude = java.lang.Double.valueOf(record[1])
        val message = record[2]
        return Accident(latitude, longitude, message)
    }

    private fun dataToRecord(data: String): Array<String> {
        return data.split(",".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()
    }
}

fun main(args: Array<String>) {
    // Using File Based
    val file = File("src/main/resources/part4/accident.info")
    val fileSource = AccidentMetadata(42L, file)
    println("File based accident data = " + fileSource.data())

    // Using URL Based
    val url = URL("https://raw.githubusercontent.com/ajoz/fp-workshop-jvm/master/src/main/resources/part4/accident.info")
    val urlSource = AccidentMetadata(24L, url)
    println("URL based accident data = " + urlSource.data())
}

internal data class Accident(
        val latitude: Double,
        val longitude: Double,
        val message: String
)