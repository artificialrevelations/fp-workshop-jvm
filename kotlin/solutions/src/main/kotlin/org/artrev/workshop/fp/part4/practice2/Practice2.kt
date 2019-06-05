@file:Suppress("PackageName")

package org.artrev.workshop.fp.part4.practice2

import org.artrev.workshop.fp.tools.control.Try
import org.artrev.workshop.fp.tools.map
import org.artrev.workshop.fp.tools.memoized
import org.artrev.workshop.fp.tools.tryGet
import java.io.File
import java.io.IOException
import java.net.URL
import java.nio.file.Files

/*
  AccidentMetadata can either be created out of an id and a file, or an id and
  url. We cannot have a situation where both File and Url is set, thus we can
  model AccidentMetadata as a simple sum type:

  AccidentMetadata = FileSource | UrlSource

  As both FileSource and UrlSource differ in the way that the data is loaded,
  we can make `loadAccident` abstract and allow each case to implement its own
  version.

  As we were asked by our clients to change the return type of `getData` to be
  more secure we can use the `Supplier.tryGet` instead of a simple `get`.

  We can go as far as changing the exceptions thrown from `loadAccident` from
  an unchecked to checked ones. The supplier can be constructed then with the
  `Supplier.ofChecked` method.
 */
internal abstract class AccidentMetadata private constructor(val id: Long) {
    private val data =
            ::loadAccident
                    .map { dataToRecord(it) }
                    .map { recordToAccident(it) }
                    .memoized()

    @Throws(Exception::class)
    protected abstract fun loadAccident(): String

    fun getData(): Try<Accident> {
        return data.tryGet()
    }

    class FileSource(id: Long, private val source: File) : AccidentMetadata(id) {
        inner class FileCorrupted internal constructor(message: String) : Exception(message)

        @Throws(FileCorrupted::class)
        override fun loadAccident(): String {
            try {
                val path = source.toPath()
                val bytes = Files.readAllBytes(path)
                return String(bytes)
            } catch (e: IOException) {
                throw FileCorrupted("Error when reading: ${source.absolutePath}")
            }
        }
    }

    class UrlSource(id: Long, private val source: URL) : AccidentMetadata(id) {
        inner class URLCorrupted internal constructor(message: String) : Exception(message)

        @Throws(URLCorrupted::class)
        override fun loadAccident(): String {
            try {
                return source
                        .openConnection()
                        .getInputStream()
                        .use {
                            String(it.readBytes())
                        }
            } catch (exception: IOException) {
                throw URLCorrupted("Error while reading URL: ${source.path}")
            }
        }
    }

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
    val fileSource = AccidentMetadata.FileSource(42L, file)
    println("File based accident data = " + fileSource.getData())

    // Using URL Based
    val url = URL("https://raw.githubusercontent.com/ajoz/fp-workshop-jvm/master/src/main/resources/part4/accident.info")
    val urlSource = AccidentMetadata.UrlSource(42L, url)
    println("URL based accident data = " + urlSource.getData())
}

// Moved to the back because we already know this class
internal data class Accident(
        val latitude: Double,
        val longitude: Double,
        val message: String
)