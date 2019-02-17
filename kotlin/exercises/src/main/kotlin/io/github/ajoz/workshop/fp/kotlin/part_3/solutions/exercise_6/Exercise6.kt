@file:Suppress("PackageName", "UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.solutions.exercise_6

internal sealed class Maybe<out A> {
    abstract val isPresent: Boolean
    abstract fun get(): A

    abstract fun <B> map(mapper: (A) -> B): Maybe<B>
    abstract fun <B> flatMap(mapper: (A) -> Maybe<B>): Maybe<B>

    object None : Maybe<Nothing>() {
        override fun <B> map(mapper: (Nothing) -> B) = this
        override fun <B> flatMap(mapper: (Nothing) -> Maybe<B>): Maybe<B> = this

        override val isPresent = false
        override fun get() =
                throw NoSuchElementException("Cannot get value from None!")
    }

    class Some<A>(private val value: A) : Maybe<A>() {
        override fun <B> map(mapper: (A) -> B) = Some(mapper(value))
        override fun <B> flatMap(mapper: (A) -> Maybe<B>) = mapper(value)

        override val isPresent = true
        override fun get() = value
    }
}

internal fun <A> Maybe<A>.getOrElse(default: A): A = when (this) {
    is Maybe.None -> default
    is Maybe.Some -> get()
}

internal class DeviceAPI {
    // this might return a null :-(
    val deviceInfo: DeviceInfo? = DeviceInfo()
    val safeDeviceInfo: Maybe<DeviceInfo> = Maybe.Some(DeviceInfo())
}

internal class DeviceInfo {
    // this might return a null :-(
    val hardwareInfo: HardwareInfo? = HardwareInfo()
    val safeHardwareInfo: Maybe<HardwareInfo> = Maybe.Some(HardwareInfo())
}

internal class HardwareInfo {
    // this might return a null :-(
    val architecture: Architecture? = Architecture.VLIW
    val safeArchitecture: Maybe<Architecture> = Maybe.Some(Architecture.VLIW)
}

internal enum class Architecture {
    VLIW,
    CISC,
    RISC,
    MISC,
    ZISC,
    EPIC
}

internal fun logging1(api: DeviceAPI) {
    val message1 =
            api.deviceInfo
                    ?.hardwareInfo
                    ?.architecture
                    ?.name
                    ?: "No architecture info available!"
    println(message1)

    var message2 = "No architecture info available!"
    val deviceInfo = api.deviceInfo
    if (null != deviceInfo) {
        val hardwareInfo = deviceInfo.hardwareInfo
        if (null != hardwareInfo) {
            val architecture = hardwareInfo.architecture
            if (null != architecture) {
                message2 = "Architecture: ${architecture.name}"
            }
        }
    }

    println(message2)
}

internal fun logging2(api: DeviceAPI) {
    var message = "No architecture info available!"
    val deviceInfo = api.safeDeviceInfo
    if (deviceInfo.isPresent) {
        val hardwareInfo = deviceInfo.get().safeHardwareInfo
        if (hardwareInfo.isPresent) {
            val architecture = hardwareInfo.get().safeArchitecture
            if (architecture.isPresent) {
                message = "Architecture: ${architecture.get().name}"
            }
        }
    }

    println(message)
}

internal fun logging3(api: DeviceAPI) {
    val message = api.safeDeviceInfo
            .flatMap { it.safeHardwareInfo }
            .flatMap { it.safeArchitecture }
            .map { "Architecture: $it" }
            .getOrElse("Architecture info is not available!")
    println(message)
}

fun main(args: Array<String>) {
    // Part 1:
    logging1(DeviceAPI())

    // Part 2:
    logging2(DeviceAPI())

    // Part 3:
    logging3(DeviceAPI())
}