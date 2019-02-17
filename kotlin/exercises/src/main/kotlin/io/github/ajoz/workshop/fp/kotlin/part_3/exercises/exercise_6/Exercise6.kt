@file:Suppress("PackageName", "UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_6

import java.util.NoSuchElementException

/*
  -- Working effectively with optionality --

  In the previous exercise we defined a type called Maybe. We used it to express
  partial-nes of a function. Let's examine first a bit enhanced version of the
  Maybe type:
  - `isPresent` method returns true if Maybe is Some and false otherwise
  - `get` method returns value stored in Some and throws exception in None
 */
internal sealed class Maybe<out A> {
    abstract val isPresent: Boolean
    abstract fun get(): A

    object None : Maybe<Nothing>() {

        override val isPresent = false
        override fun get() =
                throw NoSuchElementException("Cannot get value from None!")
    }

    class Some<A>(private val value: A) : Maybe<A>() {
        override val isPresent = true
        override fun get() = value
    }
}

/*
  Kotlin variance does not allow us to make this as a method of Maybe
  - if we want the None to be an object we need to specify Maybe<out A>
    so None is Maybe<Nothing>
  - if we do the thing above then we cannot use A in an `in` position so it is
    not possible to use it as an arg of `getOrElse` method :-(
  - if we would make the None as class None<A> : Maybe<A> instead of an object
    then we could have the `getOrElse` as a method
  */
internal fun <A> Maybe<A>.getOrElse(default: A): A =
        TODO("Exercise 6 Maybe.getOrElse is missing!")

/*
  You are working on a API for some device SDK. Currently you are working on
  function that will log information about the Architecture of the device's
  underlying hardware. Fortunately the SDK API allows to retrieve the
  information, there is just one catch. The SDK supports multiple vendors, it
  is possible that some information is not available :-(

  The SDK is built around several parts.
  - DeviceAPI which is the entry point for anything related with the SDK
  - DeviceInfo which holds the information about the underlying hardware
  - HardwareInfo which holds the information about the architecture

  Depending on the vendor anything can be null (everything except the DeviceAPI).

  Look at the classes to get familiar with the SDK design:
 */
internal class DeviceAPI {
    // this might return a null :-(
    val deviceInfo: DeviceInfo? = DeviceInfo()
}

internal class DeviceInfo {
    // this might return a null :-(
    val hardwareInfo: HardwareInfo? = HardwareInfo()
}

internal class HardwareInfo {
    // this might return a null :-(
    val architecture: Architecture? = Architecture.VLIW
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
  Part 1:

  Please create function `logging1` that takes a DeviceAPI as an argument
  and logs to console the architecture of the device. Please remember that
  everything except the passed DeviceAPI can be null.

  Please compare the solution when:
  - using `?.` and `?:`
  - using plain `if` statements

  Questions:
  - which version of the code do you like more?
  - which version feels more familiar?
  - does the code in the `if` version look like an arrow?
 */
internal fun logging1(api: DeviceAPI) {
    throw UnsupportedOperationException("Exercise 6 logging1 is missing!")
}

/*
  Part 2:

  Please modify the SDK classes:
  - to DeviceAPI add a `safeGetDeviceInfo` that returns a Maybe<DeviceInfo>
  - to DeviceInfo add a `safeGetHardwareInfo` that returns a Maybe<HardwareInfo>
  - to HardwareInfo add a `safeGetArchitecture` that returns a Maybe<Architecture>

  Please create a function `logging2` does the same thing as `logging1` but
  this time it uses "safe" versions of the methods.

  Questions:
  - does it look better or worse then the null version?
  - is it better then `?.` version from `logging1`?
  - was it worth the trouble?
 */
internal fun logging2(api: DeviceAPI) {
    throw UnsupportedOperationException("Exercise 6 logging2 is missing!")
}

/*
  Part 3:

  Please look closely at the part 2 there is an operation that is happening
  several times here:

  1. We return a Maybe<A> from some method
  2. We check if the value is present
  3. We take out the value
  4. We transform the value into another thing

  If we squint our eyes a bit we could say that Maybe<A> looks like a List<A>
  that can be empty or have just one element. We already created a function
  that works similarly to what we described above for a List.

  It was called `map`. Let's think how the `map` worked:
  - for the empty List it returned an empty List
  - for the single element List it took the value, applied the function to it and
    returned a new single element List with the result of the function.

  We could implement the same in Maybe<A>, there is a problem though. The
  function `map` was designed to work with total functions:

  fun map(list: List<A>, function: (A) -> B): List<B>

  So a signature for the Maybe type would look like:

  fun map(list: Maybe<A>, function: (A) -> B): Maybe<B>

  But we want to work with partial functions like (A) -> Maybe<B>.

  If we would use the `map` with a partial function we would get
  Maybe<Maybe<B>> as a result, it would be nice to somehow "flatten" the
  Maybe's into a single one.

  Let's define such flattening map:

  fun flatMap(maybe: Maybe<A>, function: (A) -> Maybe<B>): Maybe<B>

  Please modify the Maybe type:
  - add `map` method, for Some it should use the passed function to calculate
    the result and wrap it inside another Some, for None it should return
    None
  - add `flatMap` method, for Some it should return the result of the passed
    function, for None it should return None
  - add `getOrElse` method that should take a value as an argument, for Some
    it should return the value stored in Some, for None it should return
    value passed as an argument.

  Please create function `logging3` that is doing the same as previous
  two functions `logging1` and `logging2` but use the newly created `map`,
  `flatMap` and `getOrElse` methods.

  Can the message string be final?

  Hints:
  - you can chain the calls to `flatMap` and `getOrElse` to get a single
    expression
 */
internal fun logging3(api: DeviceAPI) {
    throw UnsupportedOperationException("Execise 5 logging3 is missing!")
}

fun main(args: Array<String>) {
    // Part 1:
    logging1(DeviceAPI())

    // Part 2:
    logging2(DeviceAPI())

    // Part 3:
    logging3(DeviceAPI())
}