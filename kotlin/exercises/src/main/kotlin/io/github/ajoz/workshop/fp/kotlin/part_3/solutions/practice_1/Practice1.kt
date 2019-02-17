@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_3.solutions.practice_1

import java.util.*

internal sealed class Try<A> {
    class Success<A>(private val value: A) : Try<A>() {
        override fun get() = value
        override fun getOrElse(default: A) = value

        override fun <B> map(function: (A) -> B): Try<B> =
                Try.success(function(value))

        override fun <B> flatMap(function: (A) -> Try<B>): Try<B> =
                function(value)

        override fun recover(function: (Throwable) -> A): Try<A> =
                Try.success(value)

        override fun recoverWith(function: (Throwable) -> Try<A>): Try<A> =
                Try.success(value)
    }

    class Failure<A>(private val error: Throwable) : Try<A>() {
        override fun get() = throw NoSuchElementException("Failure has no value!")
        override fun getOrElse(default: A) = default

        override fun <B> map(function: (A) -> B): Try<B> =
                Try.failure(error)

        override fun <B> flatMap(function: (A) -> Try<B>): Try<B> =
                Try.failure(error)

        override fun recover(function: (Throwable) -> A): Try<A> =
                Try.success(function(error))

        override fun recoverWith(function: (Throwable) -> Try<A>): Try<A> =
                function(error)
    }

    abstract fun get(): A

    abstract fun getOrElse(default: A): A

    abstract fun <B> map(function: (A) -> B): Try<B>

    abstract fun <B> flatMap(function: (A) -> Try<B>): Try<B>

    abstract fun recover(function: (Throwable) -> A): Try<A>

    abstract fun recoverWith(function: (Throwable) -> Try<A>): Try<A>

    companion object {
        fun <A> success(value: A): Try<A> {
            return Success(value)
        }

        fun <A> failure(error: Throwable): Try<A> {
            return Failure(error)
        }

        fun <A> ofNullable(value: A?): Try<A> {
            return if (null != value) {
                Try.success(value)
            } else
                Try.failure(IllegalArgumentException("Null value passed to ofNullable!"))
        }
    }
}

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
            Try.failure(RuntimeException("Architecture not specified!"))
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

internal fun tryLogging(api: DeviceAPI?) {
    val message =
            Try.ofNullable(api)
                    .flatMap {
                        it.getDeviceInfo()
                    }
                    .flatMap {
                        it.getHardwareInfo()
                    }
                    .flatMap {
                        it.getArchitecture()
                    }
                    .map {
                        it.name
                    }
                    .recover {
                        it.message ?: "Exception had no message"
                    }
                    .get()
    println(message)
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