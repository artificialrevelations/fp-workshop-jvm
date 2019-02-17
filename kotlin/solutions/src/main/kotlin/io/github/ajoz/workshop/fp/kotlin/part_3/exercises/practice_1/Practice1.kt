@file:Suppress("PackageName", "UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.practice_1


import java.util.*

/*
  -- Working effectively with failure --

  The sum type Maybe<A> we introduced in the exercises was very nice but it has
  a one major flaw. Although it allows indicating that there was an error, it
  eats the reason. There is no real way of communicating why the error occurred.

  Lets create a better type that will allow us to do that:

  data Try a = Success a | Failure Throwable

  A Try<A> is a sum type just like Maybe<A> but is capable of storing information
  about the exception (error situation) that caused the computation to fail.

  In this exercise create the type Try<A> with methods (like in Maybe<A>):
  - `map`
  - `flatMap`
  - `get`
  - `getOrElse`

  New methods that you should add:
  - `recoverWith` that takes a Function1<Throwable, Try<A>> and allows recovering
    from a failed computation. Think of it like "flatMap" but for Failure.
  - `recover` that takes a Function1<Throwable, A> and allows recovering from a
    failed computation. Think about it like a "map" but for Failure.

  For easier work with the type two static methods were added:
  - static `success` that takes a value and returns a Try.Success of this value
  - static `failure` that takes a Throwable and returns a Try.Failure of this
    error

    We will do some practice with the Device SDK example we worked on in
    Exercise 6.

    Below is the Try type that needs implementation:
 */
internal sealed class Try<out A> {
    companion object {
        fun <A> success(value: A): Try<A> =
                TODO("Practice1 Try.success is missing!")

        fun <A> failure(error: Throwable): Try<A> =
                TODO("Practice1 Try.failure is missing!")

        fun <A> ofChecked(supplier: () -> A): Try<A> {
            return try {
                Try.success(supplier())
            } catch (error: Throwable) {
                Try.failure(error)
            }

        }

        fun <A> ofNullable(value: A?): Try<A> = value
                ?.let { Try.success(it) }
                ?: Try.failure(IllegalArgumentException("Null value passed to ofNullable!"))
    }
}

/*
  In this version all the Device SDK classes return Try<A>. A simple logic was
  added to simulate a possibility of an exception happening while the code is
  run.

  Please check the implementation of the classes below and main method in the
  Practice1 class.
 */
internal class DeviceAPI {
    private val random = Random()
    private val deviceInfo = DeviceInfo()

    fun getDeviceInfo(): Try<DeviceInfo> {
        return if (random.nextBoolean()) {
            Try.success(deviceInfo)
        } else
            Try.failure(NoSuchElementException("DeviceInfo does not exist!"))
    }
}

internal class DeviceInfo {
    private val random = Random()
    private val hardwareInfo = HardwareInfo()

    fun getHardwareInfo(): Try<HardwareInfo> {
        return if (random.nextBoolean()) {
            Try.success(hardwareInfo)
        } else
            Try.failure(IllegalStateException("HardwareInfo unavailable!"))
    }
}

internal class HardwareInfo {
    private val random = Random()
    private val architecture = Architecture.VLIW

    fun getArchitecture(): Try<Architecture> {
        return if (random.nextBoolean()) {
            Try.success(architecture)
        } else
            Try.failure(RuntimeException("Architecture is not specified!"))
    }
}

internal enum class Architecture {
    VLIW,
    CISC,
    RISC,
    MISC,
    ZISC,
    EPIC
}

/*
  Please create function `tryLogging`:
  - DeviceAPI passed as argument can be null this time
  - Print the current architecture like in the exercises
  - If the architecture is unavailable (missing DeviceInfo, missing HardwareInfo,
    missing Architecture) print the error message instead

  Hints:
  - What should be the order of operations?
  - When should the error recovery happen?
 */
internal fun tryLogging(api: DeviceAPI?) {
    throw UnsupportedOperationException("Practice1 tryLogging is missing!")
}

fun main(args: Array<String>) {
    // with null:
    tryLogging(null)

    // without null:
    val api = DeviceAPI()
    for (i in 0..99) {
        tryLogging(api)
    }
}


